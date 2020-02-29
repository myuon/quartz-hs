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

prepare :: Monad m => Lexeme -> TokenConsumer s ExprFragment m ()
prepare l = do
  st <- get
  put $ l : st

expectWith
  :: Monad m => (Token -> Bool) -> TokenConsumer s ExprFragment m Lexeme
expectWith f = do
  lex <- consume
  case tokenOfLexeme lex of
    t' | f t' -> return lex
    _         -> do
      prepare lex
      throwE $ "Unexpected token: " ++ show lex

expect :: Monad m => Token -> TokenConsumer s ExprFragment m Lexeme
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

argument :: TokenConsumer s ExprFragment (ST s) ()
argument = do
  expect TLParen
  report FArgumentStart
  many $ expr >> expect TComma
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
  many $ expr >> expect TComma
  expect TRBracket

  args <- popUntil (== FArrayStart)
  report $ FExpr $ ArrayLit $ map (\(FExpr e) -> e) $ reverse args

  return ()

exprShort :: TokenConsumer s ExprFragment (ST s) ()
exprShort =
  (do
      expect TLParen
      expr
      expect TRParen

      return ()
    )
    <|> (do
          var <|> literal <|> self
          many $ member <|> argument <|> indexArray

          return ()
        )
    <|> arrayLit
 where
  var = do
    lex <- consume
    case tokenOfLexeme lex of
      TVar v -> report $ FExpr $ Var Nothing (Id [v])
      _      -> do
        prepare lex
        throwE $ "Unexpected token: " ++ show lex

  self = do
    lex <- consume
    case tokenOfLexeme lex of
      TSelf -> report $ FExpr $ Self SelfType
      _     -> do
        prepare lex
        throwE $ "Unexpected token: " ++ show lex

  literal = do
    lex <- consume
    case tokenOfLexeme lex of
      TInt    n -> report $ FExpr $ Lit $ IntLit n
      TStrLit s -> report $ FExpr $ Lit $ StringLit s
      _         -> do
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

  Just (FExpr k) <- PBV.pop stack
  return $ do
    result
    return k

parser :: [Lexeme] -> Either String (Decl AlexPosn)
parser = undefined

parserDecls :: [Lexeme] -> Either String [Decl AlexPosn]
parserDecls = undefined
