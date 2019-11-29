module Language.Quartz (
  module Language.Quartz.AST,
  evalE,
  evalD,
  runMain,
  runEvalE,
  parseExpr,
  parseDecl,
  parseModule,
  typecheck,
  runModule,
) where

import Control.Error
import Control.Monad.IO.Class
import Data.Bifunctor
import Language.Quartz.AST
import Language.Quartz.Lexer
import Language.Quartz.Parser
import Language.Quartz.TypeCheck
import Language.Quartz.Eval

parseExpr :: String -> Either String Expr
parseExpr = parserExpr . alexScanTokens

parseDecl :: String -> Either String Decl
parseDecl = parser . alexScanTokens

parseModule :: String -> Either String [Decl]
parseModule = parserDecls . alexScanTokens

data CompilerError
  = ParseError String
  | TypeCheckError TypeCheckExceptions
  | EvalError RuntimeExceptions
  deriving (Eq, Show)

runModule :: MonadIO m => String -> m (Either CompilerError Expr)
runModule s = runExceptT $ do
  decls <- withExceptT ParseError $ ExceptT $ return $ parseModule s
  withExceptT TypeCheckError $ runTypeCheckModule decls
  withExceptT EvalError $ runMain decls
