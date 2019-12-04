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
import qualified Data.Primitive.Array as Array

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
  Lit      _   -> True
  ClosureE _   -> True
  OpenE    _   -> True
  Unit         -> True
  Array _      -> True
  RecordOf _ _ -> True
  _            -> False


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
  | Unreachable Expr
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
      ClosureE (Closure (ArgTypes tyvars fargs ret) fbody) ->
        let fbody' = foldl' (uncurry . subst) fbody
              $ zipWith (\(x, _) y -> (x, y)) fargs xs'
        in  evalE $ case () of
              _ | length fargs == length xs' -> fbody'
              _ | length fargs > length xs'  -> ClosureE $ Closure
                (ArgTypes tyvars (drop (length xs') fargs) ret)
                fbody'
              _ -> FnCall fbody' (drop (length fargs) xs')
      _ -> lift $ throwE $ Unreachable vm
  Let x t -> do
    f <- evalE t
    modify $ \ctx -> ctx { exprs = M.insert x f (exprs ctx) }
    return Unit
  Match t brs -> fix
    ( \cont brs -> case brs of
      []            -> lift $ throwE PatternExhausted
      ((pat, b):bs) -> do
        ctx0   <- get
        result <- runExceptT $ execStateT (match pat t) ctx0
        case result of
          Left  _    -> cont bs
          Right ctx' -> put ctx' >> evalE b
    )
    brs
  Procedure es -> foldl' (\m e -> m >> evalE e) (return Unit) es
  FFI p es     -> get >>= \ctx -> do
    pf <- lift $ ffi ctx M.!? p ?? NotFound p
    lift $ mapExceptT liftIO $ withExceptT FFIExceptions $ pf $ map toDyn es
  ArrayLit es -> do
    let arr = Array.fromList es
    marr <- liftIO $ Array.thawArray arr 0 (Array.sizeofArray arr)
    return $ Array $ MArray marr
  IndexArray e1 e2 -> do
    arr <- evalE e1
    i   <- evalE e2
    case (arr, i) of
      (Array m, Lit (IntLit i)) -> liftIO $ Array.readArray (getMArray m) i
      _                         -> lift $ throwE $ Unreachable vm
  ForIn var e1 es -> do
    arr <- evalE e1
    ctx <- get
    case arr of
      (Array m) ->
        forM_ [0 .. Array.sizeofMutableArray (getMArray m) - 1] $ \i -> do
          r <- liftIO $ Array.readArray (getMArray m) i
          modify $ \ctx -> ctx { exprs = M.insert (Id [var]) r (exprs ctx) }
          mapM_ evalE es
      _ -> lift $ throwE $ Unreachable vm
    put ctx

    return Unit
  If brs -> fix
    ( \cont brs -> case brs of
      []                  -> return Unit
      ((cond, br):others) -> do
        result <- evalE cond
        case result of
          Lit (BoolLit True ) -> evalE br
          Lit (BoolLit False) -> cont others
    )
    brs
  Op op e1 e2 -> do
    r1 <- evalE e1
    r2 <- evalE e2
    case op of
      Eq ->
        return $ if r1 == r2 then Lit (BoolLit True) else Lit (BoolLit False)
  Member e1 v1 -> do
    r1 <- evalE e1
    case r1 of
      RecordOf _ fields -> do
        return $ (\(Just x) -> x) $ lookup v1 fields
  _ -> lift $ throwE $ Unreachable vm

runEvalE :: MonadIO m => Expr -> ExceptT RuntimeExceptions m Expr
runEvalE m = evalStateT (evalE m) std

evalD :: MonadIO m => Decl -> StateT Context (ExceptT RuntimeExceptions m) ()
evalD decl = go [] decl
 where
  go path decl = case decl of
    Enum name fs -> do
      forM_ fs $ \(EnumField f typs) -> do
        bs <- mapM (\_ -> fresh) typs
        let vars = map (Var . Id . return) bs
        modify $ \ctx -> ctx
          { exprs = M.insert
              (Id [name, f])
              ( ClosureE
                (Closure (ArgTypes [] (zip bs typs) NoType) (EnumOf name vars))
              )
            $ exprs ctx
          }

    Record d _ ->
      modify $ \ctx -> ctx { decls = PathTree.insert [d] decl (decls ctx) }
    Func d body ->
      modify $ \ctx ->
        ctx { exprs = M.insert (Id [d]) (ClosureE body) (exprs ctx) }
    Method d _ ->
      modify $ \ctx -> ctx { decls = PathTree.insert [d] decl (decls ctx) }
    ExternalFunc name (ArgTypes tyvars args ret) -> do
      bs <- mapM (\_ -> fresh) args
      let args' = zipWith (\b (_, t) -> (b, t)) bs args

      evalD $ Func
        name
        ( Closure (ArgTypes tyvars args' ret)
                  (FFI (Id [name]) (map (\n -> Var (Id [n])) $ map fst args'))
        )

std :: Context
std = Context {ffi = Std.ffi, exprs = M.empty, decls = PathTree.empty}

runMain :: MonadIO m => [Decl] -> ExceptT RuntimeExceptions m Expr
runMain decls = flip evalStateT std $ do
  mapM_ evalD decls
  evalE (FnCall (Var (Id ["main"])) [Unit])
