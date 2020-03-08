{-# LANGUAGE RankNTypes #-}
module Language.Quartz.Parser where

import           Control.Applicative
import           Control.Error
import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Vector.PushBack          as PBV
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String  ( renderString )
import           Language.Quartz.Lexer
import           Language.Quartz.AST

data Fragment
  = FExpr (Expr AlexPosn)
  | FDecl (Decl AlexPosn)
  | FArgumentStart
  | FArgument String (Maybe Type)
  | FArguments ArgType
  | FArrayStart
  | FBlockStart
  | FIdent String
  | FStatements [Expr AlexPosn]
  | FBranchStart
  | FIfBranch (Expr AlexPosn) (Expr AlexPosn)
  | FMatchBranch Pattern (Expr AlexPosn)
  | FPattern Pattern
  | FLit Literal
  | FPattenStart
  | FRecordField String (Expr AlexPosn)
  | FRecordDef RecordField
  | FRecordStart
  | FGenericsStart
  | FGenerics [String]
  | FType Type
  | FTypeApplyStart
  | FEnumField EnumField
  | FDeclStart
  | FModule [Decl AlexPosn]
  | FInterfaceStart
  | FFuncDecl String FuncType
  | FNSIdent Id
  | FNSIdentStart
  deriving (Eq, Show)

type TokenConsumer s e m a
  = ExceptT
      String
      (ReaderT (PBV.PBVector s e) (StateT ([Lexeme], AlexPosn) m))
      a

getLexPos :: Monad m => TokenConsumer s e m AlexPosn
getLexPos = gets snd

consume :: Monad m => TokenConsumer s e m Lexeme
consume = do
  (lexs, _) <- get
  case lexs of
    (t@(Lexeme p _) : ts) -> do
      put (ts, p)
      return t
    _ -> throwE "Given Lexeme stream is exhausted"

peekToken :: Monad m => TokenConsumer s e m (Maybe Lexeme)
peekToken = do
  (lexs, _) <- get
  case lexs of
    (t : _) -> return $ Just t
    _       -> return Nothing

prepare :: Monad m => Lexeme -> TokenConsumer s e m ()
prepare l = modify (\(x, y) -> (l : x, y))

expectWith :: Monad m => (Token -> Bool) -> TokenConsumer s e m ()
expectWith f = do
  lex <- consume
  case tokenOfLexeme lex of
    t' | f t' -> return ()
    _         -> do
      prepare lex
      throwE $ "Unexpected token: " ++ show lex

expect :: Monad m => Token -> TokenConsumer s e m ()
expect t = expectWith (== t)

report :: e -> TokenConsumer s e (ST s) ()
report e = do
  v <- ask
  PBV.push v e

pop :: TokenConsumer s e (ST s) (Maybe e)
pop = ask >>= PBV.pop

popUntil :: (e -> Bool) -> TokenConsumer s e (ST s) [e]
popUntil f = pop >>= \case
  Just e | f e -> return []
  Nothing      -> return []
  Just e       -> fmap (e :) $ popUntil f

requireEof :: Monad m => TokenConsumer s e m ()
requireEof = do
  ls <- get
  if null ls then return () else throwE $ "Expected eof, but got " ++ show ls

manyUntil
  :: Monad m => Token -> TokenConsumer s e m a -> TokenConsumer s e m [a]
manyUntil t m =
  (do
      a <- m
      expect t
      as <- manyUntil t m <|> return []
      return (a : as)
    )
    <|> return []

markSourcePosition :: Int -> AlexPosn -> String -> Doc a
markSourcePosition area (AlexPn _ y x) source =
  let s = (y - area) `max` 1
  in  vcat $ map (\(s, n) -> n <+> pretty "|" <+> s) $ zip
        (insert (area + 1)
                (pretty (concat $ replicate (x - 1) " ") <> pretty "^")
                (map pretty (drop (s - 1) $ lines source))
        )
        ( insert (area + 1) (pretty $ replicate (length $ show s) ' ')
        $ take (area * 2 + 1)
        $ map pretty [s ..]
        )
  where insert n x xs = let (ys, zs) = splitAt n xs in ys ++ x : zs

runParser
  :: Show e
  => (forall s . TokenConsumer s e (ST s) ())
  -> [Lexeme]
  -> String
  -> Either String e
runParser def lexs source = runST $ do
  stack               <- PBV.new 0
  (result, (rest, _)) <-
    flip runStateT (lexs, AlexPn 0 0 0) $ flip runReaderT stack $ runExceptT def

  unless (null rest) $ fail $ "Parse Error (tokens): \n\n" ++ renderString
    (layoutPretty defaultLayoutOptions
                  (markSourcePosition 2 (posOfLexeme $ head rest) source)
    )

  len <- PBV.length stack
  when (len /= 1) $ do
    rs <- PBV.toList stack
    fail $ "Parse Error (stack elements): " ++ show rs

  Just k <- PBV.pop stack
  return $ fmap (const k) result

--

mayGenerics :: TokenConsumer s Fragment (ST s) [String]
mayGenerics =
  (do
      generics
      Just (FGenerics gs) <- pop

      return gs
    )
    <|> return []

ident :: TokenConsumer s Fragment (ST s) ()
ident = do
  lex <- consume
  case tokenOfLexeme lex of
    TVar v -> report $ FIdent v
    _      -> do
      prepare lex
      throwE $ "Unexpected token: " ++ show lex

namespaceIdent :: TokenConsumer s Fragment (ST s) ()
namespaceIdent = do
  ident
  report FNSIdentStart
  void $ many $ expect TColon2 >> ident

  ps              <- popUntil (== FNSIdentStart)
  Just (FIdent p) <- pop
  report $ FNSIdent (Id $ (p :) $ map (\(FIdent v) -> v) $ reverse ps)

ifBlock :: TokenConsumer s Fragment (ST s) ()
ifBlock = do
  expect TIf
  report FBranchStart
  expect TLBrace
  void $ manyUntil TComma $ do
    expr
    expect TDArrow
    expr

    Just (FExpr e) <- pop
    Just (FExpr b) <- pop
    report $ FIfBranch b e
  expect TRBrace

  brs <- popUntil (== FBranchStart)
  report
    $ FExpr
    $ ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0)
    $ If
    $ map (\(FIfBranch x y) -> (x, y))
    $ reverse brs

pat :: TokenConsumer s Fragment (ST s) ()
pat = do
  -- terminals
  pany <|> pident <|> pliteral

  void $ many $ papply

 where
  pident = do
    namespaceIdent
    Just (FNSIdent v) <- pop
    report $ FPattern $ PVar v

  pliteral = do
    literal
    Just (FLit l) <- pop
    report $ FPattern $ PLit l

  pany = do
    expect TUnderscore
    report $ FPattern PAny

  papply = do
    expect TLParen
    report FPattenStart
    void $ manyUntil TComma pat
    expect TRParen

    ps                <- popUntil (== FPattenStart)
    Just (FPattern p) <- pop
    report $ FPattern $ PApp p $ map (\(FPattern p') -> p') $ reverse ps

match :: TokenConsumer s Fragment (ST s) ()
match = do
  expect TMatch
  exprShort
  report FBranchStart
  expect TLBrace
  void $ manyUntil TComma $ do
    pat
    expect TDArrow
    expr

    Just (FExpr    e) <- pop
    Just (FPattern p) <- pop
    report $ FMatchBranch p e
  expect TRBrace

  brs            <- popUntil (== FBranchStart)
  Just (FExpr e) <- pop
  report
    $ FExpr
    $ ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0)
    $ Match e
    $ map (\(FMatchBranch x y) -> (x, y))
    $ reverse brs

statements :: TokenConsumer s Fragment (ST s) ()
statements = do
  expect TLBrace
  report FBlockStart
  void $ many statement

  -- 最後のstatementはsemicolonがなくてよい
  -- expr自体はexprStatementでparse済みなのでそれを利用する
  mayExpr <- pop >>= \case
    Just (FExpr e) -> return $ Just (FExpr e)
    Just e         -> report e >> return Nothing
    _              -> return Nothing

  expect TRBrace

  es <- popUntil (== FBlockStart)
  report $ FStatements $ map (\(FExpr e') -> e') $ reverse $ maybe id
                                                                   (:)
                                                                   mayExpr
                                                                   es

 where
  statement =
    for <|> ifBlock <|> match <|> letStatement <|> assignment <|> exprStatement

  exprStatement = do
    -- この前にassignmentでexpr_shortまでのパースは済ませているため、recursion partのみ実行することも必要になる(Adhocすぎるのでやめたい…)
    exprLongTerminals <|> return ()
    exprRecursion
    expect TSemiColon

    Just (FExpr e) <- pop
    p              <- getLexPos
    report $ FExpr $ ExprLoc p p $ Stmt e

  letStatement = do
    expect TLet
    isRef <- peekToken >>= \case
      Just t | tokenOfLexeme t == TRef -> consume >> return True
      _ -> return False
    ident
    expect TEq
    expr
    expect TSemiColon

    Just (FExpr  e) <- pop
    Just (FIdent v) <- pop

    p               <- getLexPos
    if isRef
      then report $ FExpr $ ExprLoc p p $ Stmt $ ExprLoc p p $ LetRef v e
      else report $ FExpr $ ExprLoc p p $ Stmt $ ExprLoc p p $ Let (Id [v]) e

  assignment = do
    exprShort
    expect TEq
    expr
    expect TSemiColon

    Just (FExpr e1) <- pop
    Just (FExpr e2) <- pop
    p               <- getLexPos
    report $ FExpr $ ExprLoc p p $ Stmt $ ExprLoc p p $ Assign e2 e1

  for = do
    expect TFor
    ident
    expect TIn
    exprShort
    statements

    Just (FStatements s) <- pop
    Just (FExpr       e) <- pop
    Just (FIdent      v) <- pop
    p                    <- getLexPos
    report $ FExpr $ ExprLoc p p $ ForIn v e s

generics :: TokenConsumer s Fragment (ST s) ()
generics = do
  expect TLBracket
  report FGenericsStart
  void $ manyUntil TComma ident
  expect TRBracket

  vs <- popUntil (== FGenericsStart)
  report $ FGenerics $ map (\(FIdent v) -> v) $ reverse vs

literal :: TokenConsumer s Fragment (ST s) ()
literal = do
  lex <- consume
  case tokenOfLexeme lex of
    TInt    n -> report $ FLit $ IntLit n
    TStrLit s -> report $ FLit $ StringLit s
    TTrue     -> report $ FLit $ BoolLit True
    TFalse    -> report $ FLit $ BoolLit False
    _         -> do
      prepare lex
      throwE $ "Unexpected token: " ++ show lex

typ :: TokenConsumer s Fragment (ST s) ()
typ = do
  unit <|> self <|> var <|> ref
  void $ many $ typApply

 where
  unit = do
    expect TLParen
    expect TRParen
    report $ FType $ ConType (Id ["unit"])

  self = do
    expect TSelf
    report $ FType SelfType

  var = do
    ident
    Just (FIdent v) <- pop
    report $ FType $ ConType (Id [v])

  ref = do
    expect TRef
    expect TLBracket
    typ
    expect TRBracket

    Just (FType t) <- pop
    report $ FType t

  typApply = do
    expect TLBracket
    report FTypeApplyStart
    void $ manyUntil TComma typ
    expect TRBracket

    ts             <- popUntil (== FTypeApplyStart)
    Just (FType t) <- pop

    report $ FType $ AppType t $ map (\(FType t') -> t') ts

funcHeader
  :: Bool -> TokenConsumer s Fragment (ST s) ([String], ArgType, Maybe Type)
funcHeader mustGenerics = do
  gs <- if mustGenerics
    then
      (do
        generics
        Just (FGenerics gs) <- pop
        return gs
      )
    else mayGenerics
  funcArguments

  mayReturnType <-
    (do
        expect TColon
        typ

        Just (FType t) <- pop
        return $ Just t
      )
      <|> return Nothing

  Just (FArguments as) <- pop

  return (gs, as, mayReturnType)

 where
  funcArguments :: TokenConsumer s Fragment (ST s) ()
  funcArguments = do
    expect TLParen
    (maySelf, cont) <-
      (do
          isRef <- (expect TRef >> return True) <|> return False
          expect TSelf

          cont <- (expect TComma >> return True) <|> return False
          return (Just isRef, cont)
        )
        <|> return (Nothing, True)

    vs <- if cont
      then do
        report FArgumentStart
        void $ manyUntil TComma $ do
          ident
          mayType <-
            (do
                expect TColon
                typ

                Just (FType t) <- pop
                return $ Just t
              )
              <|> return Nothing

          Just (FIdent v) <- pop
          report $ FArgument v mayType

        popUntil (== FArgumentStart)
      else return []

    expect TRParen

    report
      $ FArguments
      $ ArgType (maybe False id maySelf) (maybe False (const True) maySelf)
      $ map (\(FArgument s t) -> (s, maybe NoType id t))
      $ reverse
      $ vs

exprShort :: TokenConsumer s Fragment (ST s) ()
exprShort = do
  -- terminals
  var <|> litE <|> self <|> parenExpr <|> statementBlock <|> arrayLit <|> deref

  -- 左再帰部
  void $ many $ member <|> argument <|> indexArray
 where
  var = do
    namespaceIdent
    Just (FNSIdent v) <- pop
    p                 <- getLexPos
    report $ FExpr $ ExprLoc p p $ Var v

  litE = do
    literal
    Just (FLit l) <- pop
    p             <- getLexPos
    report $ FExpr $ ExprLoc p p $ Lit l

  parenExpr = do
    expect TLParen
    expr
    expect TRParen

  deref = do
    expect TStar
    expr

    Just (FExpr e) <- pop
    p              <- getLexPos
    report $ FExpr $ ExprLoc p p $ Deref e

  self = do
    lex <- consume
    p   <- getLexPos
    case tokenOfLexeme lex of
      TSelf -> report $ FExpr $ ExprLoc p p $ Self SelfType
      _     -> do
        prepare lex
        throwE $ "Unexpected token: " ++ show lex

  member = do
    expect TDot
    var

    Just (FExpr (  ExprLoc p _ (Var (Id [n])))) <- pop
    Just (FExpr v@(ExprLoc _ q _             )) <- pop
    report $ FExpr $ ExprLoc p q $ Member v n

  argument = do
    expect TLParen
    report FArgumentStart
    void $ manyUntil TComma expr
    expect TRParen

    args           <- popUntil (== FArgumentStart)
    Just (FExpr f) <- pop
    p              <- getLexPos
    report $ FExpr $ ExprLoc p p $ FnCall f $ map (\(FExpr e) -> e) $ reverse
      args

    return ()

  indexArray = do
    expect TLBracket
    expr
    expect TRBracket

    Just (FExpr e1) <- pop
    Just (FExpr e2) <- pop
    p               <- getLexPos
    report $ FExpr $ ExprLoc p p $ IndexArray e2 e1

  arrayLit = do
    expect TArrayLit
    report FArrayStart
    expect TLBracket
    void $ manyUntil TComma expr
    expect TRBracket

    args <- popUntil (== FArrayStart)
    p    <- getLexPos
    report $ FExpr $ ExprLoc p p $ ArrayLit $ map (\(FExpr e) -> e) $ reverse
      args

  statementBlock = do
    statements
    Just (FStatements st) <- pop
    p                     <- getLexPos

    report $ FExpr $ ExprLoc p p $ Procedure st

exprRecursion :: TokenConsumer s Fragment (ST s) ()
exprRecursion = record <|> (void $ many operators)
 where
  -- recordはexpr_shortのvarにマッチさせてから取る(かなりAdhocなのでやめたいが…)
  record = do
    expect TLBrace
    report FRecordStart

    void $ many $ do
      ident
      expect TColon
      expr

      expect TComma <|> return ()

      Just (FExpr  e) <- pop
      Just (FIdent v) <- pop
      report $ FRecordField v e

    expect TRBrace

    fs <- popUntil (== FRecordStart)
    Just (FExpr (ExprLoc _ _ (Var (Id [v])))) <- pop
    p  <- getLexPos
    report
      $ FExpr
      $ ExprLoc p p
      $ RecordOf v
      $ map (\(FRecordField x y) -> (x, y))
      $ reverse fs

  operators = foldl1
    (<|>)
    [ binOp TPlus   Add
    , binOp TMinus  Sub
    , binOp TStar   Mult
    , binOp TSlash  Div
    , binOp TLeq    Leq
    , binOp TLAngle Lt
    , binOp TGeq    Geq
    , binOp TRAngle Gt
    , binOp TEq2    Eq
    ]

  binOp t op = do
    expect t
    expr

    Just (FExpr e1) <- pop
    Just (FExpr e2) <- pop
    p               <- getLexPos
    report $ FExpr $ ExprLoc p p $ Op op e2 e1

exprLongTerminals :: TokenConsumer s Fragment (ST s) ()
exprLongTerminals = match <|> ifBlock <|> lambdaAbs
 where
  lambdaAbs = do
    (vs, as, mayReturnType) <- funcHeader True
    expect TDArrow
    expr

    Just (FExpr e) <- pop
    p              <- getLexPos

    report $ FExpr $ ExprLoc p p $ ClosureE
      (Closure
        (FuncType vs as (maybe (ConType (Id ["unit"])) id mayReturnType))
        e
      )

expr :: TokenConsumer s Fragment (ST s) ()
expr = do
  -- terminals
  exprLongTerminals <|> exprShort

  -- recursion part
  exprRecursion

parserExpr :: [Lexeme] -> String -> Either String (Expr AlexPosn)
parserExpr lexs source = do
  k <- runParser expr lexs source
  case k of
    FExpr e -> return e
    _       -> Left $ "Parse Error (unexpected fragment): " ++ show k

--

decl :: TokenConsumer s Fragment (ST s) ()
decl = externalFunc <|> func <|> enum <|> record <|> interface <|> derive
 where
  externalFunc = do
    expect TExternal
    expect TFunc
    ident
    (gs, as, ret) <- funcHeader False
    expect TSemiColon

    Just (FIdent v) <- pop

    report $ FDecl $ ExternalFunc
      v
      (FuncType gs as (maybe (ConType (Id ["unit"])) id ret))

  func = do
    expect TFunc
    ident
    (gs, as, ret) <- funcHeader False
    statements

    Just (FStatements st) <- pop
    Just (FIdent      v ) <- pop
    p                     <- getLexPos

    report $ FDecl $ Func
      v
      ( Closure (FuncType gs as (maybe (ConType (Id ["unit"])) id ret))
      $ ExprLoc p p
      $ Procedure st
      )

  enum = do
    expect TEnum
    ident
    gs <- mayGenerics
    expect TLBrace
    report FBlockStart
    void $ manyUntil TComma $ do
      ident
      typs <-
        (do
            expect TLParen
            report FTypeApplyStart
            void $ manyUntil TComma typ
            expect TRParen

            ts <- popUntil (== FTypeApplyStart)
            return $ reverse ts
          )
          <|> return []

      Just (FIdent v) <- pop
      report $ FEnumField $ EnumField v $ map (\(FType t) -> t) typs
    expect TRBrace

    bs              <- popUntil (== FBlockStart)
    Just (FIdent v) <- pop
    report $ FDecl $ Enum v gs $ map (\(FEnumField e) -> e) $ reverse bs

  record = do
    expect TRecord
    ident
    gs <- mayGenerics
    expect TLBrace
    report FBlockStart
    void $ manyUntil TComma $ do
      ident
      expect TColon
      typ

      Just (FType  t) <- pop
      Just (FIdent v) <- pop
      report $ FRecordDef $ RecordField v t
    expect TRBrace

    bs              <- popUntil (== FBlockStart)
    Just (FIdent v) <- pop

    report $ FDecl $ Record v gs $ map (\(FRecordDef d) -> d) $ reverse bs

  interface = do
    expect TInterface
    ident
    gs <- mayGenerics
    expect TLBrace
    report FInterfaceStart
    void $ many $ do
      expect TFunc
      ident
      (gs, as, ret) <- funcHeader False
      expect TSemiColon

      Just (FIdent v) <- pop
      report
        $ FFuncDecl v (FuncType gs as (maybe (ConType (Id ["unit"])) id ret))
    expect TRBrace

    fs              <- popUntil (== FInterfaceStart)
    Just (FIdent v) <- pop
    report $ FDecl $ Interface v gs $ map (\(FFuncDecl x y) -> (x, y)) $ reverse
      fs

  derive = do
    expect TDerive
    ident
    gs      <- mayGenerics
    forType <-
      (do
          expect TFor
          typ

          Just (FType t) <- pop
          return $ Just t
        )
        <|> return Nothing
    expect TLBrace
    decls
    expect TRBrace

    Just (FModule ds) <- pop
    Just (FIdent  v ) <- pop
    report $ FDecl $ Derive v gs forType ds

parser :: [Lexeme] -> String -> Either String (Decl AlexPosn)
parser lexs source = do
  k <- runParser decl lexs source
  case k of
    FDecl e -> return e
    _       -> Left $ "Parse Error (unexpected fragment): " ++ show k

decls :: TokenConsumer s Fragment (ST s) ()
decls = do
  report FDeclStart
  void $ many decl

  ds <- popUntil (== FDeclStart)
  report $ FModule $ map (\(FDecl d) -> d) $ reverse ds

parserDecls :: [Lexeme] -> String -> Either String [Decl AlexPosn]
parserDecls lexs source = do
  k <- runParser decls lexs source
  case k of
    FModule e -> return e
    _         -> Left $ "Parse Error (unexpected fragment): " ++ show k

