module Language.Quartz.Parser where

import           Control.Applicative
import           Control.Error
import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Vector.PushBack          as PBV
import           Language.Quartz.Lexer
import           Language.Quartz.AST

data ExprFragment
  = FExpr (Expr AlexPosn)
  | FArgumentStart
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
  deriving (Eq, Show)

type TokenConsumer s e m a
  = ExceptT String (ReaderT (PBV.PBVector s e) (StateT [Lexeme] m)) a

consume :: Monad m => TokenConsumer s ExprFragment m Lexeme
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

prepare :: Monad m => Lexeme -> TokenConsumer s ExprFragment m ()
prepare l = do
  st <- get
  put $ l : st

expectWith :: Monad m => (Token -> Bool) -> TokenConsumer s ExprFragment m ()
expectWith f = do
  lex <- consume
  case tokenOfLexeme lex of
    t' | f t' -> return ()
    _         -> do
      prepare lex
      throwE $ "Unexpected token: " ++ show lex

expect :: Monad m => Token -> TokenConsumer s ExprFragment m ()
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

--

ident :: TokenConsumer s ExprFragment (ST s) ()
ident = do
  lex <- consume
  case tokenOfLexeme lex of
    TVar v -> report $ FIdent v
    _      -> do
      prepare lex
      throwE $ "Unexpected token: " ++ show lex

argument :: TokenConsumer s ExprFragment (ST s) ()
argument = do
  expect TLParen
  report FArgumentStart
  void $ many $ expr >> expect TComma
  expect TRParen

  args           <- popUntil (== FArgumentStart)
  Just (FExpr f) <- pop
  report $ FExpr $ FnCall f $ map (\(FExpr e) -> e) $ reverse args

  return ()

arrayLit :: TokenConsumer s ExprFragment (ST s) ()
arrayLit = do
  expect TArrayLit
  report FArrayStart
  expect TLBracket
  void $ many $ expr >> expect TComma
  expect TRBracket

  args <- popUntil (== FArrayStart)
  report $ FExpr $ ArrayLit $ map (\(FExpr e) -> e) $ reverse args

ifBlock :: TokenConsumer s ExprFragment (ST s) ()
ifBlock = do
  expect TIf
  report FBranchStart
  expect TLBrace
  void $ many $ do
    exprShort
    expect TDArrow
    expr
    expect TComma

    Just (FExpr e) <- pop
    Just (FExpr b) <- pop
    report $ FIfBranch b e
  expect TRBrace

  brs <- popUntil (== FBranchStart)
  report $ FExpr $ If $ map (\(FIfBranch x y) -> (x, y)) $ reverse brs

pat :: TokenConsumer s ExprFragment (ST s) ()
pat = do
  -- terminals
  pany <|> pident <|> pliteral

  void $ many $ papply

 where
  pident = do
    ident
    Just (FIdent v) <- pop
    report $ FPattern $ PVar (Id [v])

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
    void $ many $ do
      pat
      expect TComma <|> return ()
    expect TRParen

    ps                <- popUntil (== FPattenStart)
    Just (FPattern p) <- pop
    report $ FPattern $ PApp p $ map (\(FPattern p') -> p') $ reverse ps

match :: TokenConsumer s ExprFragment (ST s) ()
match = do
  expect TMatch
  exprShort
  report FBranchStart
  expect TLBrace
  void $ many $ do
    pat
    expect TDArrow
    expr
    expect TComma

    Just (FExpr    e) <- pop
    Just (FPattern p) <- pop
    report $ FMatchBranch p e
  expect TRBrace

  brs            <- popUntil (== FBranchStart)
  Just (FExpr e) <- pop
  report $ FExpr $ Match e $ map (\(FMatchBranch x y) -> (x, y)) $ reverse brs

statements :: TokenConsumer s ExprFragment (ST s) ()
statements = do
  expect TLBrace
  report FBlockStart
  void $ many statement
  expr <|> report (FExpr Unit)
  expect TRBrace

  Just e <- pop
  es     <- popUntil (== FBlockStart)
  report
    $  FExpr
    $  Procedure
    $  map ((,) Nothing)
    $  map (\(FExpr e) -> e)
    $  reverse es
    ++ [e]

 where
  statement =
    for <|> ifBlock <|> match <|> letStatement <|> assignment <|> exprStatement

  exprStatement = do
    expr
    expect TSemiColon

    Just (FExpr e) <- pop
    report $ FExpr $ Stmt e

  letStatement = do
    expect TLet
    isRef <- peekToken >>= \case
      Just t | tokenOfLexeme t == TRef -> return True
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

generics :: TokenConsumer s ExprFragment (ST s) ()
generics = undefined

literal :: TokenConsumer s ExprFragment (ST s) ()
literal = do
  lex <- consume
  case tokenOfLexeme lex of
    TInt    n -> report $ FLit $ IntLit n
    TStrLit s -> report $ FLit $ StringLit s
    _         -> do
      prepare lex
      throwE $ "Unexpected token: " ++ show lex

exprShort :: TokenConsumer s ExprFragment (ST s) ()
exprShort = do
  -- terminals
  var <|> litE <|> self <|> parenExpr <|> statements <|> arrayLit <|> deref

  -- 左再帰部
  void $ many $ member <|> argument <|> indexArray
 where
  var = do
    ident
    Just (FIdent v) <- pop
    report $ FExpr $ Var Nothing (Id [v])

  litE = do
    literal
    Just (FLit l) <- pop
    report $ FExpr $ Lit l

  parenExpr = do
    expect TLParen
    expr
    expect TRParen

    return ()

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

  indexArray = do
    expect TLBracket
    expr
    expect TRBracket

    Just (FExpr e1) <- pop
    Just (FExpr e2) <- pop
    report $ FExpr $ IndexArray e2 e1

expr :: TokenConsumer s ExprFragment (ST s) ()
expr = do
  -- terminals
  exprShort <|> match <|> ifBlock <|> lambdaAbs
 where
  lambdaAbs = do
    generics
    argument
    -- return type
    expect TDArrow
    expr

parserExpr :: [Lexeme] -> Either String (Expr AlexPosn)
parserExpr lexs = runST $ do
  stack          <- PBV.new 0
  (result, rest) <- flip runStateT lexs $ flip runReaderT stack $ runExceptT
    expr

  unless (null rest) $ fail $ "Parse Error (tokens): " ++ show rest

  len <- PBV.length stack
  when (len /= 1) $ do
    rs <- PBV.toList stack
    fail $ "Parse Error (stack elements): " ++ show rs

  Just (FExpr k) <- PBV.pop stack
  return $ do
    result
    return k

parser :: [Lexeme] -> Either String (Decl AlexPosn)
parser = undefined

parserDecls :: [Lexeme] -> Either String [Decl AlexPosn]
parserDecls = undefined
