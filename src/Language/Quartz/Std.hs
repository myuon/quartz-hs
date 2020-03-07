module Language.Quartz.Std where

import           Control.Monad.IO.Class
import           Control.Error
import qualified Data.Map                      as M
import           Data.Dynamic
import           Language.Quartz.AST
import           Language.Quartz.Lexer                    ( AlexPosn(..) )
import qualified Data.Primitive.Array          as Array

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
      case unwrapExpr expr of
        Lit (StringLit s) -> liftIO $ putStrLn s
        Lit (IntLit    n) -> liftIO $ print n
        EnumOf _ _        -> liftIO $ print expr
        _                 -> liftIO $ print expr

      return $ ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) Unit
    )
  , ( Id ["range"]
    , \[s, e] -> do
      x <- (fromDynamic s :: Maybe (Expr AlexPosn)) ?? InvalidExpr s
      y <- (fromDynamic e :: Maybe (Expr AlexPosn)) ?? InvalidExpr e
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit ix), Lit (IntLit iy)) -> do
          let arr = Array.fromList $ map
                (ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) . Lit . IntLit)
                [ix .. iy]
          marr <- liftIO $ Array.thawArray arr 0 (Array.sizeofArray arr)
          return $ srcSpanExpr x y $ Array $ MArray marr
        _ -> throwE $ InvalidExpr s
    )
  , ( Id ["mod"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (IntLit (x' `mod` y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["add_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (IntLit (x' + y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["subtract_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (IntLit (x' - y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["mult_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (IntLit (x' * y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["div_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (IntLit (x' `div` y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["leq_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (BoolLit (x' <= y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["lt_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (BoolLit (x' < y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["geq_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (BoolLit (x' >= y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["gt_int"]
    , \[d1, d2] -> do
      x <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      y <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr x, unwrapExpr y) of
        (Lit (IntLit x'), Lit (IntLit y')) ->
          return $ srcSpanExpr x y $ Lit (BoolLit (x' > y'))
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["length_array"]
    , \[d1] -> do
      arr <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      case unwrapExpr arr of
        (Array (MArray marr)) ->
          return $ srcSpanExpr arr arr $ Lit $ IntLit $ Array.sizeofMutableArray
            marr
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["grow_array"]
    , \[d1, d2] -> do
      arr  <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      size <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr arr, unwrapExpr size) of
        (Array (MArray marr), Lit (IntLit n)) -> do
          marr' <-
            liftIO
            $ Array.newArray (Array.sizeofMutableArray marr + n)
            $ srcSpanExpr arr arr Unit
          liftIO $ Array.copyMutableArray marr'
                                          0
                                          marr
                                          0
                                          (Array.sizeofMutableArray marr)
          return $ srcSpanExpr arr size $ Array $ MArray marr'
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["concat_string"]
    , \[d1, d2] -> do
      arr  <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      size <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      case (unwrapExpr arr, unwrapExpr size) of
        (Lit (StringLit s1), Lit (StringLit s2)) -> do
          return $ srcSpanExpr arr size $ Lit $ StringLit $ s1 ++ s2
        _ -> throwE $ InvalidExpr d1
    )
  , ( Id ["int_to_string"]
    , \[d1] -> do
      e1 <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      case unwrapExpr e1 of
        Lit (IntLit n) -> do
          return $ srcSpanExpr e1 e1 $ Lit $ StringLit $ show n
        _ -> throwE $ InvalidExpr d1
    )
  ]
