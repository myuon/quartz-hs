module Language.Quartz.Parser where

import           Control.Error
import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Vector.PushBack          as PBV
import           Language.Quartz.Lexer
import           Language.Quartz.AST

type TokenConsumer s e m a
  = ExceptT String (ReaderT (PBV.PBVector s e) (StateT [Lexeme] m)) a

consume :: Monad m => TokenConsumer s (Expr AlexPosn) m (Maybe Lexeme)
consume = do
  lexs <- get
  case lexs of
    (t : ts) -> do
      put ts
      return $ Just t
    _ -> return Nothing

prepare :: Monad m => Maybe Lexeme -> TokenConsumer s (Expr AlexPosn) m ()
prepare lex = case lex of
  Just l -> do
    st <- get
    put $ l : st
  Nothing -> return ()

expect :: Monad m => Token -> TokenConsumer s (Expr AlexPosn) m (Maybe Lexeme)
expect t = do
  lex <- consume
  case fmap tokenOfLexeme lex of
    Just t' | t' == t -> return lex
    _                 -> prepare lex >> return Nothing

report :: e -> TokenConsumer s e (ST s) ()
report e = do
  v <- ask
  PBV.push v e

pop :: TokenConsumer s e (ST s) (Maybe e)
pop = do
  v <- ask
  PBV.pop v

--

exprShortTerminals
  :: TokenConsumer s (Expr AlexPosn) (ST s) (Maybe (Expr AlexPosn))
exprShortTerminals = do
  lex <- consume
  case fmap tokenOfLexeme lex of
    Just (TInt    n) -> return $ Just $ Lit $ IntLit n
    Just (TStrLit s) -> return $ Just $ Lit $ StringLit s
    Just (TVar    n) -> return $ Just $ Var Nothing (Id [n])
    Just TSelf       -> return $ Just $ Self SelfType
    _                -> return Nothing

exprShort :: TokenConsumer s (Expr AlexPosn) (ST s) ()
exprShort = do
  e1 <- exprShortTerminals
  forM_ e1 report

parserExpr :: [Lexeme] -> Either String (Expr AlexPosn)
parserExpr lexs = runST $ do
  stack          <- PBV.new 0
  (result, rest) <-
    flip runStateT lexs $ flip runReaderT stack $ runExceptT $ do
      exprShort

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
