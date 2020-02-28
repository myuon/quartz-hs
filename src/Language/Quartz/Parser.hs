module Language.Quartz.Parser where

import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad.State
import qualified Data.Vector.PushBack          as PBV
import           Language.Quartz.Lexer
import           Language.Quartz.AST

data StackElement s
  = STokens (PBV.PBVector s (Expr AlexPosn))
  | SCode (Expr AlexPosn)

type TokenConsumer m a = StateT [Lexeme] m a

pop :: Monad m => TokenConsumer m (Maybe Lexeme)
pop = do
  lexs <- get
  case lexs of
    (t : ts) -> do
      put ts
      return $ Just t
    _ -> return Nothing

pushMaybe :: Monad m => Maybe Lexeme -> TokenConsumer m ()
pushMaybe lex = case lex of
  Just l -> do
    st <- get
    put $ l : st
  Nothing -> return ()

literal :: TokenConsumer (ST s) (Maybe Literal)
literal = do
  lex <- pop
  case fmap tokenOfLexeme lex of
    Just (TInt    n) -> return $ Just $ IntLit n
    Just (TStrLit s) -> return $ Just $ StringLit s
    _                -> pushMaybe lex >> return Nothing

var :: TokenConsumer (ST s) (Maybe (Expr AlexPosn))
var = do
  lex <- pop
  case fmap tokenOfLexeme lex of
    Just (TVar n) -> return $ Just $ Var Nothing (Id [n])
    Nothing       -> return Nothing
    _             -> pushMaybe lex >> return Nothing

parserExpr :: [Lexeme] -> Either String (Expr AlexPosn)
parserExpr lexs =
  let (a, rest) = runST $ flip runStateT lexs $ do
        v   <- var
        lit <- literal
        return $ fmap Lit lit <|> v
  in  if not (null rest)
        then Left ("parse failed: " ++ show rest)
        else case a of
          Nothing -> Left "rule not found"
          Just a  -> return a

parser :: [Lexeme] -> Either String (Decl AlexPosn)
parser = undefined

parserDecls :: [Lexeme] -> Either String [Decl AlexPosn]
parserDecls = undefined
