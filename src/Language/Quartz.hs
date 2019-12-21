module Language.Quartz (
  module Language.Quartz.AST,
  evalE,
  evalD,
  runMain,
  runEvalE,
  parseExpr,
  parseDecl,
  parseModule,
  typecheckExpr,
  typecheckModule,
  runExpr,
  runModule,
) where

import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Language.Quartz.AST
import Language.Quartz.Lexer
import Language.Quartz.Parser
import Language.Quartz.Renamer
import Language.Quartz.TypeCheck
import Language.Quartz.Eval

parseExpr :: String -> Either String (Expr AlexPosn)
parseExpr = parserExpr . alexScanTokens

parseDecl :: String -> Either String (Decl AlexPosn)
parseDecl = parser . alexScanTokens

parseModule :: String -> Either String [Decl AlexPosn]
parseModule = parserDecls . alexScanTokens

data CompilerError
  = ParseError String
  | TypeCheckError TypeCheckExceptions
  | EvalError RuntimeExceptions
  deriving Show

runExpr :: MonadIO m => String -> m (Either CompilerError (Expr AlexPosn))
runExpr s =
  runExceptT
    $   ($ s)
    $   (withExceptT ParseError . ExceptT . return . parseExpr)
    >=> (return . transformVarConTypeE)
    >=> (withExceptT TypeCheckError . runTypeCheckExpr)
    >=> (withExceptT EvalError . runEvalE)

runModule :: MonadIO m => String -> m (Either CompilerError (Expr AlexPosn))
runModule s =
  runExceptT
    $   ($ s)
    $   (withExceptT ParseError . ExceptT . return . parseModule)
    >=> (return . map transformVarConTypeD)
    >=> (withExceptT TypeCheckError . runTypeCheckModule)
    >=> (withExceptT EvalError . runMain)
