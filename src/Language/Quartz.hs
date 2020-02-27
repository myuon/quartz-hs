{-# LANGUAGE TemplateHaskell #-}
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
  runModuleWith,
) where

import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Dynamic
import Data.FileEmbed
import qualified Data.Map as M
import Language.Quartz.AST
import Language.Quartz.Lexer
import Language.Quartz.Parser
import Language.Quartz.Transform
import Language.Quartz.TypeCheck
import Language.Quartz.Eval
import Language.Quartz.Std
import Paths_quartz

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
runExpr s = do
  std <- liftIO $ readFile =<< getDataFileName "lib/std.qz"

  -- stdを読み込む必要があるので一旦moduleに送るという強引な手段を取る
  runExceptT
    $   ($ std ++ "func main[A](): A {" ++ s ++ "}")
    $   (withExceptT ParseError . ExceptT . return . parseModule)
    >=> (return . map (transformSelfTypeD . transformVarConTypeD . desugarOpD))
    >=> (withExceptT TypeCheckError . runTypeCheckModule)
    >=> (withExceptT EvalError . runMainWith M.empty)

{-
  runExceptT
    $   ($ s)
    $   (withExceptT ParseError . ExceptT . return . parseExpr)
    >=> (return . transformVarConTypeE . desugarOpE)
    >=> (withExceptT TypeCheckError . runTypeCheckExpr)
    >=> (withExceptT EvalError . runEvalE)
-}

runModule :: MonadIO m => String -> m (Either CompilerError (Expr AlexPosn))
runModule = runModuleWith M.empty

runModuleWith
  :: MonadIO m
  => M.Map Id ([Dynamic] -> ExceptT FFIExceptions m (Expr AlexPosn))
  -> String
  -> m (Either CompilerError (Expr AlexPosn))
runModuleWith ffi s = do
  std <- liftIO $ readFile =<< getDataFileName "lib/std.qz"

  runExceptT
    $   ($ std ++ s)
    $   (withExceptT ParseError . ExceptT . return . parseModule)
    >=> (return . map (transformSelfTypeD . transformVarConTypeD . desugarOpD))
    >=> (withExceptT TypeCheckError . runTypeCheckModule)
    >=> (withExceptT EvalError . runMainWith ffi)
