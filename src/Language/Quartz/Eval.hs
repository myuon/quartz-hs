{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Quartz.Eval where

import Control.Applicative
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Error
import qualified Data.Map as M
import Data.Dynamic
import Data.Foldable
import Language.Quartz.AST
import Language.Quartz.TypeCheck (fresh, argumentOf)
import qualified Language.Quartz.Std as Std
import qualified Data.PathTree as PathTree

data Context = Context {
  decls :: PathTree.PathTree String Decl,
  exprs :: M.Map Id Expr,
  ffi :: M.Map Id ([Dynamic] -> ExceptT Std.FFIExceptions IO Expr)
}

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
  FFI p es     -> FFI p $ map (\x -> subst x var term) es
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
  Unit        -> True
  FFI _ _     -> False


match
  :: MonadIO m
  => Pattern
  -> Expr
  -> StateT Context (ExceptT RuntimeExceptions m) ()
match pat term = evalE term >>= \t' -> case (pat, t') of
  (PVar p, t) ->
    modify $ \ctx -> ctx { exprs = M.insert (Id [p]) t (exprs ctx) }
  (PLit lit, Lit lit') ->
    lift $ assertMay (lit == lit') ?? PatternNotMatch (PLit lit) term
  (PApp pf pxs, FnCall f xs) ->
    match pf f >> mapM_ (uncurry match) (zip pxs xs)
  (PAny, _) -> return ()

data RuntimeExceptions
  = NotFound Id
  | PatternNotMatch Pattern Expr
  | PatternExhausted
  | FFIExceptions Std.FFIExceptions
  deriving Show

-- Assume renaming is done
evalE :: MonadIO m => Expr -> StateT Context (ExceptT RuntimeExceptions m) Expr
evalE vm = case vm of
  _ | isNormalForm vm -> return vm
  Var t               -> get >>= \ctx -> lift $ exprs ctx M.!? t ?? NotFound t
  FnCall f xs         -> do
    f'  <- evalE f
    xs' <- mapM evalE xs

    case f' of
      _ | null xs' -> return f'
      ClosureE (Closure _ fargs fbody) ->
        let fbody' = foldl' (uncurry . subst) fbody $ zip fargs xs'
        in  evalE $ case () of
              _ | length fargs == length xs' -> fbody'
              _ | length fargs > length xs' ->
                ClosureE $ Closure NoType (drop (length xs') fargs) fbody'
              _ -> FnCall fbody' (drop (length fargs) xs')
      _ -> evalE $ FnCall f' xs'
  Let x t -> do
    f <- evalE t
    modify $ \ctx -> ctx { exprs = M.insert x f (exprs ctx) }
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
  Procedure es -> foldl' (\m e -> m >> evalE e) (return NoExpr) es
  FFI p es     -> get >>= \ctx -> do
    pf <- lift $ ffi ctx M.!? p ?? NotFound p
    lift $ mapExceptT liftIO $ withExceptT FFIExceptions $ pf $ map toDyn es

runEvalE :: MonadIO m => Expr -> ExceptT RuntimeExceptions m Expr
runEvalE m = evalStateT (evalE m) std

evalD :: MonadIO m => Decl -> StateT Context (ExceptT RuntimeExceptions m) ()
evalD decl = go [] decl
 where
  go path decl = case decl of
    Enum d _ ->
      modify $ \ctx -> ctx { decls = PathTree.insert [d] decl (decls ctx) }
    Record d _ ->
      modify $ \ctx -> ctx { decls = PathTree.insert [d] decl (decls ctx) }
    Func d body ->
      modify $ \ctx ->
        ctx { exprs = M.insert (Id [d]) (ClosureE body) (exprs ctx) }
    Method d _ ->
      modify $ \ctx -> ctx { decls = PathTree.insert [d] decl (decls ctx) }
    ExternalFunc f c -> do
      let (args, _) = argumentOf c
      bs <- mapM (\_ -> fresh) args

      evalD $ Func f (Closure c bs (FFI (Id [f]) (map (\n -> Var (Id [n])) bs)))

std :: Context
std = Context {ffi = Std.ffi, exprs = M.empty, decls = PathTree.empty}

runMain :: MonadIO m => [Decl] -> ExceptT RuntimeExceptions m Expr
runMain decls = flip evalStateT std $ do
  mapM_ evalD decls
  evalE (FnCall (Var (Id ["main"])) [Unit])
