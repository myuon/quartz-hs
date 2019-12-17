module Language.Quartz.TypeCheck where

import Control.Error
import Control.Monad.State
import Data.Foldable
import qualified Data.PathTree as PathTree
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Quartz.AST
import Language.Quartz.Lexer (AlexPosn)
import qualified Language.Quartz.Std as Std
import Data.Unique

data Context = Context {
  schemes :: M.Map Id Scheme,
  records :: M.Map Id ([String], [(String, Type)]),
  enums :: M.Map Id [(String, [Type])],
  selfType :: Maybe Type,
  traits :: M.Map String [FuncType]
} deriving (Eq, Show)

std :: Context
std = Context
  { schemes  = M.empty
  , records  = M.empty
  , selfType = Nothing
  , enums    = M.empty
  , traits   = M.empty
  }

data TypeCheckExceptions
  = UnificationFailed Type Type
  | InfiniteType String Type
  | NotFound (Maybe AlexPosn) Id
  | TypeNotMatch Type Type
  | OccurCheck String Type
  | CannotInfer (Expr AlexPosn)
  | PatternNotMatch Pattern Type
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
  => Expr AlexPosn
  -> StateT Context (ExceptT TypeCheckExceptions m) (Subst, Type)
algoW expr = case expr of
  Var posn v -> do
    ctx  <- get
    v'   <- lift $ schemes ctx M.!? v ?? NotFound posn v
    inst <- instantiate v'
    return (emptySubst, inst)
  Lit      (IntLit    n) -> return (emptySubst, ConType (Id ["int"]))
  Lit      (DoubleLit n) -> return (emptySubst, ConType (Id ["double"]))
  Lit      (CharLit   c) -> return (emptySubst, ConType (Id ["char"]))
  Lit      (StringLit c) -> return (emptySubst, ConType (Id ["string"]))
  Lit      (BoolLit   c) -> return (emptySubst, ConType (Id ["bool"]))
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
  ClosureE (Closure (ArgTypes _ args ret) body) -> do
    (s1, t1) <- do
      modify $ \ctx -> ctx
        { schemes = foldl' (\mp (s, t) -> M.insert (Id [s]) (Scheme [] t) mp)
                           (schemes ctx)
                           args
        }
      algoW body

    s2 <- lift $ mgu ret t1
    return (s2 `compose` s1, apply s1 $ foldr ArrowType t1 $ map snd args)
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
  If brs -> do
    b      <- fresh
    substs <- forM brs $ \(cond, expr) -> do
      (s1, t1) <- algoW cond
      s2       <- lift $ mgu t1 (ConType (Id ["bool"]))

      (s3, t3) <- algoW expr
      s4       <- lift $ mgu t3 (VarType b)
      return (s4 `compose` s3 `compose` s2 `compose` s1)

    let s = foldr1 compose substs
    return (s, apply s (VarType b))
  Op op e1 e2 -> do
    (s1, t1) <- algoW e1
    (s2, t2) <- algoW e2
    case op of
      Eq -> do
        s3 <- lift $ mgu t1 t2
        return (s3 `compose` s2 `compose` s1, ConType (Id ["bool"]))
  Member e1 v1 -> do
    (s1, t1) <- algoW e1
    case t1 of
      ConType name -> do
        ctx     <- get
        (_, rc) <- lift $ records ctx M.!? name ?? NotFound Nothing name
        t2      <- lift $ lookup v1 rc ?? NotFound Nothing (Id [v1])
        return (s1, t2)
      VarType _                -> lift $ throwE $ CannotInfer e1
      AppType (ConType name) _ -> do
        ctx     <- get
        (_, rc) <- lift $ records ctx M.!? name ?? NotFound Nothing name
        t2      <- lift $ lookup v1 rc ?? NotFound Nothing (Id [v1])
        return (s1, t2)
  RecordOf name fields -> do
    ctx          <- get
    (tyvars, rc) <- lift $ records ctx M.!? (Id [name]) ?? NotFound
      Nothing
      (Id [name])
    substs <- forM fields $ \(field, expr) -> do
      typ      <- lift $ lookup field rc ?? NotFound Nothing (Id [field])
      (s1, t1) <- algoW expr
      s2       <- lift $ mgu t1 typ
      return $ s2 `compose` s1
    let s = foldr1 compose substs

    fvars <- mapM (\_ -> fresh) tyvars
    return
      ( s
      , if null tyvars
        then ConType (Id [name])
        else AppType (ConType (Id [name])) (map VarType fvars)
      )
  Match e1 brs -> do
    b        <- fresh
    (s1, t1) <- algoW e1

    substs   <- forM brs $ \(pat, expr) -> do
      s2       <- match pat t1
      (s3, t3) <- algoW expr
      s4       <- lift $ mgu t3 (VarType b)
      return $ s4 `compose` s3 `compose` s2
    let s' = foldl' compose s1 substs

    return (s', apply s' (VarType b))
  Assign e1 e2 -> do
    (s1, t1) <- algoW e1
    (s2, t2) <- algoW e2
    s3       <- lift $ mgu t1 t2
    return (s3 `compose` s2 `compose` s1, apply s3 t1)
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

  match
    :: MonadIO m
    => Pattern
    -> Type
    -> StateT Context (ExceptT TypeCheckExceptions m) Subst
  match pat typ = case (pat, typ) of
    -- PVarで0引数constructorのとき
    (PVar v, t) -> do
      modify $ \ctx -> ctx { schemes = M.insert v (Scheme [] t) $ schemes ctx }
      return emptySubst
    (PLit (IntLit    _), ConType (Id ["int"])   ) -> return emptySubst
    (PLit (StringLit _), ConType (Id ["string"])) -> return emptySubst
    (PLit (CharLit   _), ConType (Id ["char"])  ) -> return emptySubst
    (PLit (DoubleLit _), ConType (Id ["double"])) -> return emptySubst
    (PLit (BoolLit   _), ConType (Id ["bool"])  ) -> return emptySubst

    -- PAppになるのはenumのときなのでenumの分解を行う
    (PApp (PVar p1) ps , _                      ) -> do
      ctx          <- get
      Scheme _ typ <- lift $ schemes ctx M.!? p1 ?? NotFound Nothing p1
      let (args, _) = argumentOf typ
      ss <- zipWithM match ps args
      return $ foldl' compose emptySubst ss
    (PAny, _) -> return emptySubst
    _         -> lift $ throwE $ PatternNotMatch pat typ

typecheckExpr
  :: MonadIO m
  => Expr AlexPosn
  -> StateT Context (ExceptT TypeCheckExceptions m) Type
typecheckExpr e = fmap snd $ algoW e

typecheckModule
  :: MonadIO m
  => [Decl AlexPosn]
  -> StateT Context (ExceptT TypeCheckExceptions m) ()
typecheckModule ds = mapM_ check ds
 where
  typeApply []   ty = ty
  typeApply vars ty = AppType ty $ map VarType vars

  check d = case d of
    Enum name tyvars fs -> modify $ \ctx -> ctx
      { schemes = foldl'
        ( \mp (EnumField f typs) -> M.insert
          (Id [name, f])
          ( Scheme tyvars
          $ foldr ArrowType (typeApply tyvars $ ConType (Id [name])) typs
          )
          mp
        )
        (schemes ctx)
        fs
      , enums   = M.insert (Id [name]) (map (\(EnumField f ts) -> (f, ts)) fs)
        $ enums ctx
      }
    Record r tyvars rds -> modify $ \ctx -> ctx
      { records = M.insert (Id [r])
                           (tyvars, map (\(RecordField s t) -> (s, t)) rds)
        $ records ctx
      }
    Instance _ _ _ ds -> typecheckModule ds
    OpenD _           -> return ()
    Func name c@(Closure argtypes@(ArgTypes tyvars _ _) _) -> do
      b <- fresh
      modify $ \ctx -> ctx
        { schemes = M.insert (Id [name]) (Scheme tyvars (VarType b))
          $ schemes ctx
        }
      ty <- typecheckExpr (ClosureE c)
      s  <- lift $ mgu (typeOfArgs argtypes) ty
      modify $ \ctx -> ctx
        { schemes = M.insert (Id [name]) (Scheme tyvars $ apply s ty)
          $ schemes ctx
        }
    Method _ c -> typecheckExpr (ClosureE c) >> return ()
    ExternalFunc name (ArgTypes tyvars args ret) -> modify $ \ctx -> ctx
      { schemes = M.insert
          (Id [name])
          (Scheme tyvars $ foldr ArrowType ret $ map snd args)
        $ schemes ctx
      }
    Trait s _ fs ->
      modify $ \ctx -> ctx { traits = M.insert s fs $ traits ctx }

runTypeCheckExpr
  :: MonadIO m => Expr AlexPosn -> ExceptT TypeCheckExceptions m Type
runTypeCheckExpr e = evalStateT (typecheckExpr e) std

runTypeCheckModule
  :: MonadIO m => [Decl AlexPosn] -> ExceptT TypeCheckExceptions m ()
runTypeCheckModule ds = evalStateT (typecheckModule ds) std
