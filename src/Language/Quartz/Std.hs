module Language.Quartz.Std where

import Control.Monad.IO.Class
import Control.Error
import qualified Data.Map as M
import Data.Dynamic
import Language.Quartz.AST
import Language.Quartz.Lexer (AlexPosn)
import qualified Data.Primitive.Array as Array

data FFIExceptions
  = InvalidExpr Dynamic
  deriving Show

ffi
  :: MonadIO m
  => M.Map Id ([Dynamic] -> ExceptT FFIExceptions m (Expr AlexPosn))
ffi = M.fromList
  [ ( Id ["println"]
    , \[d] -> do
      expr <- (fromDynamic d :: Maybe (Expr AlexPosn)) ?? InvalidExpr d
      case expr of
        Lit (StringLit s) -> liftIO $ putStrLn s
        Lit (IntLit    n) -> liftIO $ print n
        EnumOf _ _        -> liftIO $ print expr
        _                 -> liftIO $ print expr

      return Unit
    )
  , ( Id ["range"]
    , \[s, e] -> do
      x <- (fromDynamic s :: Maybe (Expr AlexPosn)) ?? InvalidExpr s
      y <- (fromDynamic e :: Maybe (Expr AlexPosn)) ?? InvalidExpr e
      case (x, y) of
        (Lit (IntLit x), Lit (IntLit y)) -> do
          let arr = Array.fromList $ map (Lit . IntLit) [x .. y]
          marr <- liftIO $ Array.thawArray arr 0 (Array.sizeofArray arr)
          return $ Array $ MArray marr
        _ -> throwE $ InvalidExpr s
    )
  , ( Id ["mod"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ Lit (IntLit (x' `mod` y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["add_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) -> return $ Lit (IntLit (x' + y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["subtract_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) -> return $ Lit (IntLit (x' - y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["mult_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) -> return $ Lit (IntLit (x' * y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["div_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ Lit (IntLit (x' `div` y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["leq_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) -> return $ Lit (BoolLit (x' <= y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["lt_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) -> return $ Lit (BoolLit (x' < y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["geq_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) -> return $ Lit (BoolLit (x' >= y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["gt_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (x, y) of
        (Lit (IntLit x'), Lit (IntLit y')) -> return $ Lit (BoolLit (x' > y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["length_array"]
    , \[d1] -> do
      arr <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      case arr of
        (Array (MArray marr)) ->
          return $ Lit $ IntLit $ Array.sizeofMutableArray marr
        _ -> throwE $ InvalidExpr d1
    )
  ]
