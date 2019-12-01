module Language.Quartz.TypeCheck where

import Control.Error
import Control.Monad.State
import Data.Foldable
import qualified Data.PathTree as PathTree
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Quartz.AST
import qualified Language.Quartz.Std as Std
import Data.Unique

data Context = Context {
  schemes :: M.Map Id Scheme,
  selfType :: Maybe Type
}

std :: Context
std = Context M.empty Nothing

data TypeCheckExceptions
  = UnificationFailed Type Type
  | InfiniteType String Type
  | NotFound Id
  | TypeNotMatch Type Type
  | OccurCheck String Type
  deriving (Eq, Show)

argumentOf :: Type -> ([Type], Type)
argumentOf ty = go ty []
 where
  go (ArrowType t1 t2) acc = go t2 (t1 : acc)
  go t                 acc = (reverse acc, t)

newtype Subst = Subst { getSubst :: M.Map String Type }

emptySubst :: Subst
emptySubst = Subst M.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Subst $ fmap (apply s1) (getSubst s2) `M.union` getSubst s1

mgu :: MonadIO m => Type -> Type -> ExceptT TypeCheckExceptions m Subst
mgu x y = case (x, y) of
  (ArrowType t1 t2, ArrowType s1 s2) -> do
    u1 <- mgu t1 s1
    u2 <- mgu (apply u1 t2) (apply u1 s2)
    return $ u1 `compose` u2
  (VarType u    , t            )          -> varBind u t
  (t            , VarType u    )          -> varBind u t
  (ConType s    , ConType t    ) | s == t -> return emptySubst
  (AppType s1 s2, AppType t1 t2)          -> do
    u1  <- mgu s1 t1
    u2s <- zipWithM mgu s2 t2
    return $ u1 `compose` foldr' compose emptySubst u2s
  _ -> throwE $ TypeNotMatch x y
 where
  varBind u t | t == VarType u     = return emptySubst
              | u `S.member` ftv t = throwE $ OccurCheck u t
              | otherwise          = return $ Subst $ M.singleton u t

class Apply s where
  apply :: Subst -> s -> s
  ftv :: s -> S.Set String

instance Apply Scheme where
  apply s (Scheme vars t) = Scheme vars (apply (Subst $ foldr' M.delete (getSubst s) vars) t)
  ftv (Scheme vars t) = ftv t S.\\ S.fromList vars

instance Apply Context where
  apply s ctx = ctx { schemes = fmap (apply s) (schemes ctx) }
  ftv s = S.unions $ map ftv $ M.elems $ schemes s

instance Apply Type where
  apply s typ = case typ of
    VarType n -> maybe typ id $ getSubst s M.!? n
    ArrowType t1 t2 -> ArrowType (apply s t1) (apply s t2)
    t -> t
  ftv (VarType n) = S.singleton n
  ftv (ArrowType t1 t2) = S.union (ftv t1) (ftv t2)
  ftv _ = S.empty

fresh :: MonadIO m => m String
fresh = do
  n <- liftIO $ hashUnique <$> newUnique
  return $ "?" ++ show n

generalize :: Context -> Type -> Scheme
generalize ctx typ = Scheme (S.toList $ ftv typ S.\\ ftv ctx) typ

instantiate
  :: MonadIO m => Scheme -> StateT Context (ExceptT TypeCheckExceptions m) Type
instantiate (Scheme vars typ) = do
  bs <- mapM (\_ -> fmap VarType fresh) vars
  return $ apply (Subst $ M.fromList $ zip vars bs) typ

algoW
  :: MonadIO m
  => Expr
  -> StateT Context (ExceptT TypeCheckExceptions m) (Subst, Type)
algoW expr = case expr of
  Var v -> do
    ctx  <- get
    v'   <- lift $ schemes ctx M.!? v ?? NotFound v
    inst <- instantiate v'
    return (emptySubst, inst)
  Lit      (IntLit    n) -> return (emptySubst, ConType (Id ["int"]))
  Lit      (DoubleLit n) -> return (emptySubst, ConType (Id ["double"]))
  Lit      (CharLit   c) -> return (emptySubst, ConType (Id ["char"]))
  Lit      (StringLit c) -> return (emptySubst, ConType (Id ["string"]))
  ArrayLit es            -> do
    b <- fresh
    fmap (\(s, t) -> (s, AppType (ConType (Id ["array"])) [t])) $ foldlM
      ( \(s1, t1) e -> do
        (s2, t2) <- algoW e
        s3       <- lift $ mgu t1 t2
        return (s3 `compose` s2 `compose` s1, apply s3 t2)
      )
      (emptySubst, VarType b)
      es
  IndexArray arr i -> do
    (s1, t1) <- algoW arr
    b        <- fresh
    s2       <- lift $ mgu t1 (AppType (ConType (Id ["array"])) [VarType b])

    (s3, t3) <- algoW i
    s4       <- lift $ mgu t3 (ConType (Id ["int"]))
    return (s4 `compose` s3 `compose` s2 `compose` s1, apply s2 (VarType b))
  -- ここでSchemeの引数を無視しているが問題ないか？
  ClosureE (Closure (Scheme _ t) args body) -> do
    bs       <- mapM (\_ -> fmap VarType fresh) args
    (s1, t1) <- do
      modify $ \ctx -> ctx
        { schemes = foldl'
            (\ctx (a, b) -> M.insert (Id [a]) (Scheme [] b) ctx)
            (schemes ctx)
          $ zip args bs
        }
      algoW body

    let t' = foldr' ArrowType t1 bs
    s2 <- lift $ mgu t t'
    return (s2 `compose` s1, apply s2 t')
  FnCall f []   -> algoW f
  FnCall f es   -> appW $ reverse $ f : es
  Let    x expr -> do
    (s1, t1) <- algoW expr
    modify $ \ctx -> apply
      s1
      ( ctx { schemes = M.insert x (generalize (apply s1 ctx) t1) $ schemes ctx
            }
      )
    return (s1, ConType (Id ["unit"]))
  Unit         -> return (emptySubst, ConType (Id ["unit"]))
  Procedure es -> foldlM
    ( \(s1, _) e -> do
      -- discarding the previous type information
      (s2, t2) <- algoW e
      return (s2 `compose` s1, t2)
    )
    (emptySubst, ConType (Id ["unit"]))
    es
  ForIn elem arr es -> do
    b        <- fresh
    (s1, t1) <- algoW arr
    s2       <- lift $ mgu t1 (AppType (ConType (Id ["array"])) [VarType b])
    ctx      <- get
    modify $ \ctx -> ctx
      { schemes = M.insert (Id [elem]) (Scheme [] (VarType b)) $ schemes ctx
      }
    (s3, t3) <- algoW $ Procedure es
    put ctx
    s4 <- lift $ mgu t3 $ ConType (Id ["unit"])
    return (s4 `compose` s3 `compose` s2 `compose` s1, ConType (Id ["unit"]))
  If b1 e2 e3 -> do
    (s1, t1) <- algoW b1
    s2       <- lift $ mgu t1 (ConType (Id ["bool"]))
    (s3, t3) <- algoW e2
    (s4, t4) <- algoW e3
    s5       <- lift $ mgu t3 t4
    return (foldl1 compose [s5, s4, s3, s2, s1], apply s5 t3)
  Op op e1 e2 -> do
    (s1, t1) <- algoW e1
    (s2, t2) <- algoW e2
    case op of
      Eq -> do
        s3 <- lift $ mgu t1 t2
        return (s3 `compose` s2 `compose` s1, ConType (Id ["bool"]))
  _ -> error $ show expr
 where
  appW (e1:e2:[]) = do
    b        <- VarType <$> fresh
    ctx      <- get
    (s1, t1) <- algoW e2
    put $ apply s1 ctx
    (s2, t2) <- algoW e1
    put ctx
    s3 <- lift $ mgu (apply s2 t1) (ArrowType t2 b)
    return (s3 `compose` s2 `compose` s1, apply s3 b)
  appW (e1:es) = do
    b        <- VarType <$> fresh
    ctx      <- get
    (s1, t1) <- appW es
    put $ apply s1 ctx
    (s2, t2) <- algoW e1
    put ctx
    s3 <- lift $ mgu (apply s2 t1) (ArrowType t2 b)
    return (s3 `compose` s2 `compose` s1, apply s3 b)

typecheckExpr
  :: MonadIO m => Expr -> StateT Context (ExceptT TypeCheckExceptions m) Type
typecheckExpr e = fmap snd $ algoW e

typecheckModule
  :: MonadIO m => [Decl] -> StateT Context (ExceptT TypeCheckExceptions m) ()
typecheckModule ds = mapM_ check ds
 where
  check d = case d of
    Enum     _ _  -> return ()
    Record   _ _  -> return ()
    Instance _ ds -> typecheckModule ds
    OpenD _       -> return ()
    Func   _ c    -> typecheckExpr (ClosureE c) >> return ()
    Method _ c    -> typecheckExpr (ClosureE c) >> return ()
    ExternalFunc s c ->
      modify $ \ctx -> ctx { schemes = M.insert (Id [s]) c $ schemes ctx }

runTypeCheckExpr :: MonadIO m => Expr -> ExceptT TypeCheckExceptions m Type
runTypeCheckExpr e = evalStateT (typecheckExpr e) std

runTypeCheckModule :: MonadIO m => [Decl] -> ExceptT TypeCheckExceptions m ()
runTypeCheckModule ds = evalStateT (typecheckModule ds) std
