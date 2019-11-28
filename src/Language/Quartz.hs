module Language.Quartz (
  module Language.Quartz.AST,
  evalE,
  evalD,
  runMain,
  runEval,
  parseExpr,
  parseDecl,
  parseModule,
  typecheck,
) where

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
