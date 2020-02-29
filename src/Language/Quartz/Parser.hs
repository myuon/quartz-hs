module Language.Quartz.Parser where

import           Control.Applicative
import           Control.Error
import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Vector.PushBack          as PBV
import           Language.Quartz.Lexer
import           Language.Quartz.AST

type TokenConsumer s e m a
  = ExceptT String (ReaderT (PBV.PBVector s e) (StateT [Lexeme] m)) a

consume :: Monad m => TokenConsumer s (Expr AlexPosn) m Lexeme
consume = do
  lexs <- get
  case lexs of
    (t : ts) -> do
      put ts
      return t
    _ -> throwE "Given Lexeme stream is exhausted"

prepare :: Monad m => Lexeme -> TokenConsumer s (Expr AlexPosn) m ()
prepare l = do
  st <- get
  put $ l : st

expectWith
  :: Monad m => (Token -> Bool) -> TokenConsumer s (Expr AlexPosn) m Lexeme
expectWith f = do
  lex <- consume
  case tokenOfLexeme lex of
    t' | f t' -> return lex
    _         -> throwE $ "Unexpected token: " ++ show lex

expect :: Monad m => Token -> TokenConsumer s (Expr AlexPosn) m Lexeme
expect t = expectWith (== t)

report :: e -> TokenConsumer s e (ST s) ()
report e = do
  v <- ask
  PBV.push v e

pop :: TokenConsumer s e (ST s) (Maybe e)
pop = ask >>= PBV.pop

requireEof :: Monad m => TokenConsumer s e m ()
requireEof = do
  ls <- get
  if null ls then return () else throwE $ "Expected eof, but got " ++ show ls

--

argument :: TokenConsumer s (Expr AlexPosn) (ST s) ()
argument = do
  expect TLParen
  many $ expr >> expect TComma
  expect TRParen

  return ()

exprShort :: TokenConsumer s (Expr AlexPosn) (ST s) ()
exprShort = do
  var <|> literal <|> self
  many $ member <|> argument
  return ()
 where
  var = do
    lex <- consume
    case tokenOfLexeme lex of
      TVar v -> report $ Var Nothing (Id [v])
      _      -> do
        prepare lex
        throwE $ "Unexpected token: " ++ show lex

  self = do
    lex <- consume
    case tokenOfLexeme lex of
      TSelf -> report $ Self SelfType
      _     -> do
        prepare lex
        throwE $ "Unexpected token: " ++ show lex

  literal = do
    lex <- consume
    case tokenOfLexeme lex of
      TInt    n -> report $ Lit $ IntLit n
      TStrLit s -> report $ Lit $ StringLit s
      _         -> do
        prepare lex
        throwE $ "Unexpected token: " ++ show lex

  member = do
    expect TDot
    var

    Just (Var Nothing (Id [n])) <- pop
    Just v                      <- pop
    report $ Member v n

expr :: TokenConsumer s (Expr AlexPosn) (ST s) ()
expr = exprShort

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

  Just k <- PBV.pop stack
  return $ do
    result
    return k

parser :: [Lexeme] -> Either String (Decl AlexPosn)
parser = undefined

parserDecls :: [Lexeme] -> Either String [Decl AlexPosn]
parserDecls = undefined
