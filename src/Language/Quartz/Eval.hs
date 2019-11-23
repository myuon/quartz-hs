{-# LANGUAGE OverloadedStrings #-}
module Language.Quartz.Eval where

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Error
import qualified Data.Map as M
import Data.Dynamic
import Data.Foldable
import Language.Quartz.AST

newtype Context = Context { getContext :: M.Map Id Expr }
  deriving (Eq, Show)

subst :: Expr -> String -> Expr -> Expr
subst expr var term = case expr of
  Var y | y == Id [var] -> term
  FnCall f xs           -> FnCall f (map (\x -> subst x var term) xs)
  Let    x t            -> Let x (subst t var term)
  Match  e args         -> Match
    (subst e var term)
    ( map
      ( \(pat, br) -> case pat of
        PVar y -> (pat, subst br var term)
      )
      args
    )
  Procedure es -> Procedure $ map (\x -> subst x var term) es
  _            -> expr

isNormalForm :: Expr -> Bool
isNormalForm vm = case vm of
  Var      _  -> False
  Lit      _  -> True
  ClosureE _  -> True
  -- 型チェックに通っているならFnCallは必ず簡約できるはずである(ここではFFIは考えていない)
  FnCall _ _  -> False
  Let    _ _  -> False
  Match  _ _  -> False
  OpenE     _ -> True
  Procedure _ -> False


match
  :: MonadIO m
  => Pattern
  -> Expr
  -> StateT Context (ExceptT RuntimeExceptions m) ()
match pat term = evaluate term >>= \t' -> case (pat, t') of
  (PVar p, t) -> modify (Context . M.insert (Id [p]) t . getContext)
  (PLit lit, Lit lit') ->
    lift $ assertMay (lit == lit') ?? PatternNotMatch (PLit lit) term
  (PApp pf pxs, FnCall f xs) ->
    match pf f >> mapM_ (uncurry match) (zip pxs xs)
  (PAny, _) -> return ()

data RuntimeExceptions
  = NotFound Id
  | PatternNotMatch Pattern Expr
  | PatternExhausted
  deriving (Eq, Show)

-- Assume renaming is done
evaluate
  :: MonadIO m => Expr -> StateT Context (ExceptT RuntimeExceptions m) Expr
evaluate vm = case vm of
  _ | isNormalForm vm -> return vm
  Var t -> get >>= \ctx -> lift $ getContext ctx M.!? t ?? NotFound t
  FnCall f xs -> do
    f'  <- evaluate f
    xs' <- mapM evaluate xs

    case f' of
      _ | null xs' -> return f'
      ClosureE (Closure _ fargs fbody) ->
        let fbody' = foldl' (uncurry . subst) fbody $ zip fargs xs'
        in  evaluate $ case () of
              _ | length fargs == length xs' -> fbody'
              _ | length fargs > length xs' ->
                ClosureE $ Closure NoType (drop (length xs') fargs) fbody'
              _ -> FnCall fbody' (drop (length fargs) xs')
      _ -> evaluate $ FnCall f' xs'
  Let x t -> do
    f <- evaluate t
    modify (Context . M.insert x f . getContext)
    return NoExpr
  Match t brs -> fix
    ( \cont brs -> case brs of
      []            -> lift $ throwE PatternExhausted
      ((pat, b):bs) -> do
        ctx0   <- get
        result <- runExceptT $ execStateT (match pat t) ctx0
        case result of
          Left  _    -> cont bs
          Right ctx' -> put ctx' >> return b
    )
    brs
  Procedure es -> foldl' (\m e -> m >> evaluate e) (return NoExpr) es

runEvaluate :: MonadIO m => Expr -> ExceptT RuntimeExceptions m Expr
runEvaluate m = evalStateT (evaluate m) $ Context M.empty
