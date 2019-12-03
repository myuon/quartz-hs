module Language.Quartz.Std where

import Control.Monad.IO.Class
import Control.Error
import qualified Data.Map as M
import Data.Dynamic
import Language.Quartz.AST
import qualified Data.Primitive.Array as Array

data FFIExceptions
  = InvalidExpr Dynamic
  deriving Show

ffi :: M.Map Id ([Dynamic] -> ExceptT FFIExceptions IO Expr)
ffi = M.fromList
  [ ( Id ["println"]
    , \[d] -> do
      expr <- (fromDynamic d :: Maybe Expr) ?? InvalidExpr d
      case expr of
        Lit (StringLit s) -> liftIO $ putStrLn s
        Lit (IntLit    n) -> liftIO $ print n
        _                 -> throwE $ InvalidExpr d

      return Unit
    )
  , ( Id ["range"]
    , \[s, e] -> do
      x <- (fromDynamic s :: Maybe Expr) ?? InvalidExpr s
      y <- (fromDynamic e :: Maybe Expr) ?? InvalidExpr e
      case (x, y) of
        (Lit (IntLit x), Lit (IntLit y)) -> do
          let arr = Array.fromList $ map (Lit . IntLit) [x .. y]
          marr <- liftIO $ Array.thawArray arr 0 (Array.sizeofArray arr)
          return $ Array $ MArray marr
        _ -> throwE $ InvalidExpr s
    )
  , ( Id ["mod"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe Expr) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe Expr) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ Lit (IntLit (x' `mod` y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["add"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe Expr) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe Expr) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) -> return $ Lit (IntLit (x' + y'))
        _ -> throwE $ InvalidExpr d1
    )
  ]
