module Language.Quartz.TypeCheck where

import Control.Error
import Control.Monad.State
import Data.Foldable
import qualified Data.PathTree as PathTree
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Quartz.AST
import Data.Unique

newtype Context = Context { getContext :: M.Map Id Scheme }

data Scheme = Scheme [String] Type

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
  (VarType u, t        ) -> varBind u t
  (t        , VarType u) -> varBind u t
  (ConType s, ConType t) | s == t -> return emptySubst
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
  apply s = Context . fmap (apply s) . getContext
  ftv s = S.unions $ map ftv $ M.elems $ getContext s

instance Apply Type where
  apply s typ = case typ of
    VarType n -> maybe typ id $ getSubst s M.!? n
    ArrowType t1 t2 -> ArrowType (apply s t1) (apply s t2)
    t -> t
  ftv (VarType n) = S.singleton n
  ftv (ArrowType t1 t2) = S.union (ftv t1) (ftv t2)
  ftv _ = S.empty

fresh :: MonadIO m => StateT Context (ExceptT TypeCheckExceptions m) String
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
    inst <- instantiate (getContext ctx M.! v)
    return (emptySubst, inst)
  Lit      (IntLit    n        ) -> return (emptySubst, ConType (Id ["int"]))
  Lit      (DoubleLit n        ) -> return (emptySubst, ConType (Id ["double"]))
  Lit      (CharLit   c        ) -> return (emptySubst, ConType (Id ["char"]))
  Lit      (StringLit c        ) -> return (emptySubst, ConType (Id ["string"]))
  ClosureE (Closure t args body) -> do
    bs       <- mapM (\_ -> fmap VarType fresh) args
    (s1, t1) <- do
      modify $ \ctx ->
        Context
          $ foldl' (\ctx (a, b) -> M.insert (Id [a]) (Scheme [] b) ctx)
                   (getContext ctx)
          $ zip args bs
      algoW body

    let t' = foldr' (\t b -> ArrowType b t) t1 bs
    s2 <- lift $ mgu t t'
    return (s2 `compose` s1, apply s2 t')
  FnCall f []   -> algoW f
  FnCall f es   -> appW $ reverse $ f : es
  Let    x expr -> do
    (s1, t1) <- algoW expr
    modify $ \ctx -> apply
      s1
      (Context $ M.insert x (generalize (apply s1 ctx) t1) $ getContext ctx)
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

typecheck :: MonadIO m => Expr -> ExceptT TypeCheckExceptions m Type
typecheck e = fmap snd $ evalStateT (algoW e) (Context M.empty)

runTypeCheckModule :: MonadIO m => [Decl] -> ExceptT TypeCheckExceptions m ()
runTypeCheckModule ds = mapM_ check ds
 where
  check d = case d of
    Enum     _ _  -> return ()
    Record   _ _  -> return ()
    Instance _ ds -> runTypeCheckModule ds
    OpenD _       -> return ()
    Func   _ c    -> typecheck (ClosureE c) >> return ()
    Method _ c    -> typecheck (ClosureE c) >> return ()
