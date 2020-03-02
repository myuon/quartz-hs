{-# LANGUAGE RankNTypes #-}
module Language.Quartz.Parser where

import           Control.Applicative
import           Control.Error
import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Vector.PushBack          as PBV
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
  = ExceptT String (ReaderT (PBV.PBVector s e) (StateT [Lexeme] m)) a

consume :: Monad m => TokenConsumer s e m Lexeme
consume = do
  lexs <- get
  case lexs of
    (t : ts) -> do
      put ts
      return t
    _ -> throwE "Given Lexeme stream is exhausted"

peekToken :: Monad m => TokenConsumer s e m (Maybe Lexeme)
peekToken = do
  lexs <- get
  case lexs of
    (t : _) -> return $ Just t
    _       -> return Nothing

prepare :: Monad m => Lexeme -> TokenConsumer s e m ()
prepare l = modify (l :)

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

runParser
  :: Show e
  => (forall s . TokenConsumer s e (ST s) ())
  -> [Lexeme]
  -> Either String e
runParser def lexs = runST $ do
  stack          <- PBV.new 0
  (result, rest) <- flip runStateT lexs $ flip runReaderT stack $ runExceptT def

  unless (null rest) $ fail $ "Parse Error (tokens): " ++ show rest

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
  report $ FExpr $ If $ map (\(FIfBranch x y) -> (x, y)) $ reverse brs

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
  report $ FExpr $ Match e $ map (\(FMatchBranch x y) -> (x, y)) $ reverse brs

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
    report $ FExpr $ Stmt e

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

    if isRef
      then report $ FExpr $ Stmt $ LetRef v e
      else report $ FExpr $ Stmt $ Let (Id [v]) e

  assignment = do
    exprShort
    expect TEq
    expr
    expect TSemiColon

    Just (FExpr e1) <- pop
    Just (FExpr e2) <- pop
    report $ FExpr $ Stmt $ Assign e2 e1

  for = do
    expect TFor
    ident
    expect TIn
    exprShort
    statements

    Just (FStatements s) <- pop
    Just (FExpr       e) <- pop
    Just (FIdent      v) <- pop
    report $ FExpr $ ForIn v e $ map ((,) Nothing) s

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
    report $ FExpr $ Var Nothing v

  litE = do
    literal
    Just (FLit l) <- pop
    report $ FExpr $ Lit l

  parenExpr = do
    expect TLParen
    expr
    expect TRParen

  deref = do
    expect TStar
    expr

    Just (FExpr e) <- pop
    report $ FExpr $ Deref e

  self = do
    lex <- consume
    case tokenOfLexeme lex of
      TSelf -> report $ FExpr $ Self SelfType
      _     -> do
        prepare lex
        throwE $ "Unexpected token: " ++ show lex

  member = do
    expect TDot
    var

    Just (FExpr (Var Nothing (Id [n]))) <- pop
    Just (FExpr v                     ) <- pop
    report $ FExpr $ Member v n

  argument = do
    expect TLParen
    report FArgumentStart
    void $ manyUntil TComma expr
    expect TRParen

    args           <- popUntil (== FArgumentStart)
    Just (FExpr f) <- pop
    report $ FExpr $ FnCall f $ map (\(FExpr e) -> e) $ reverse args

    return ()

  indexArray = do
    expect TLBracket
    expr
    expect TRBracket

    Just (FExpr e1) <- pop
    Just (FExpr e2) <- pop
    report $ FExpr $ IndexArray e2 e1

  arrayLit = do
    expect TArrayLit
    report FArrayStart
    expect TLBracket
    void $ manyUntil TComma expr
    expect TRBracket

    args <- popUntil (== FArrayStart)
    report $ FExpr $ ArrayLit $ map (\(FExpr e) -> e) $ reverse args

  statementBlock = do
    statements
    Just (FStatements st) <- pop

    report $ FExpr $ Procedure $ map ((,) Nothing) st

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

    fs                            <- popUntil (== FRecordStart)
    Just (FExpr (Var _ (Id [v]))) <- pop
    report $ FExpr $ RecordOf v $ map (\(FRecordField x y) -> (x, y)) $ reverse
      fs

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
    report $ FExpr $ Op op e2 e1

exprLongTerminals :: TokenConsumer s Fragment (ST s) ()
exprLongTerminals = match <|> ifBlock <|> lambdaAbs
 where
  lambdaAbs = do
    (vs, as, mayReturnType) <- funcHeader True
    expect TDArrow
    expr

    Just (FExpr e) <- pop

    report $ FExpr $ ClosureE
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

parserExpr :: [Lexeme] -> Either String (Expr AlexPosn)
parserExpr lexs = do
  k <- runParser expr lexs
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

    report $ FDecl $ Func
      v
      ( Closure (FuncType gs as (maybe (ConType (Id ["unit"])) id ret))
      $ Procedure
      $ map ((,) Nothing) st
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

parser :: [Lexeme] -> Either String (Decl AlexPosn)
parser lexs = do
  k <- runParser decl lexs
  case k of
    FDecl e -> return e
    _       -> Left $ "Parse Error (unexpected fragment): " ++ show k

decls :: TokenConsumer s Fragment (ST s) ()
decls = do
  report FDeclStart
  void $ many decl

  ds <- popUntil (== FDeclStart)
  report $ FModule $ map (\(FDecl d) -> d) $ reverse ds

parserDecls :: [Lexeme] -> Either String [Decl AlexPosn]
parserDecls lexs = do
  k <- runParser decls lexs
  case k of
    FModule e -> return e
    _         -> Left $ "Parse Error (unexpected fragment): " ++ show k

