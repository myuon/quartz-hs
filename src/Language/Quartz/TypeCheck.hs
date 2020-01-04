module Language.Quartz.TypeCheck where

import Control.Applicative
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
  traits :: M.Map String [(String, FuncType)],
  impls :: M.Map String [String]
} deriving (Eq, Show)

std :: Context
std = Context
  { schemes = M.empty
  , records = M.empty
  , enums   = M.empty
  , traits  = M.empty
  , impls   = M.empty
  }

data TypeCheckExceptions
  = UnificationFailed Type Type
  | InfiniteType String Type
  | NotFound (Maybe AlexPosn) Id
  | TypeNotMatch Type Type
  | OccurCheck String Type
  | CannotInfer (Expr AlexPosn)
  | PatternNotMatch Pattern Type
  | AmbiguousName String
  | CannotDerefNonReferencedType Type
  | CannotAssignToNonReferencedType (Expr AlexPosn) Type
  deriving (Eq, Show)

argumentOf :: Type -> ([Type], Type)
argumentOf (FnType xs y) = (xs, y)
argumentOf t             = ([], t)

newtype Subst = Subst { getSubst :: M.Map String Type }
  deriving (Eq, Show)

emptySubst :: Subst
emptySubst = Subst M.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Subst $ fmap (apply s1) (getSubst s2) `M.union` getSubst s1

mgu :: MonadIO m => Type -> Type -> ExceptT TypeCheckExceptions m Subst
mgu x y = case (x, y) of
  (FnType t1 t2, FnType s1 s2) -> do
    u1s <- zipWithM mgu t1 s1
    let u1 = foldr' compose emptySubst u1s
    u2 <- mgu (apply u1 t2) (apply u1 s2)
    return $ u1 `compose` u2
  (VarType u    , t            )          -> varBind u t
  (t            , VarType u    )          -> varBind u t
  (ConType s    , ConType t    ) | s == t -> return emptySubst
  (AppType s1 s2, AppType t1 t2)          -> do
    u1  <- mgu s1 t1
    u2s <- zipWithM mgu s2 t2
    return $ u1 `compose` foldr' compose emptySubst u2s

  -- SelfTypeはメソッド呼び出し時に解決されているはずなのでここでは無視できる
  (SelfType  , _         ) -> return emptySubst
  (_         , SelfType  ) -> return emptySubst

  (RefType t1, RefType t2) -> mgu t1 t2
  _                        -> throwE $ TypeNotMatch x y
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
    FnType t1s t2 -> FnType (map (apply s) t1s) (apply s t2)
    t -> t
  ftv (VarType n) = S.singleton n
  ftv (FnType t1 t2) = S.union (S.unions $ map ftv t1) (ftv t2)
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
  -> StateT
       Context
       (ExceptT TypeCheckExceptions m)
       (Subst, Type, Expr AlexPosn)
algoW expr = case expr of
  Var posn v -> do
    ctx <- get
    case v of
      _ | v `M.member` schemes ctx -> do
        let v' = schemes ctx M.! v
        inst <- instantiate v'
        return (emptySubst, inst, expr)

      -- non-self associated method
      Id [typ, name] | isJust (lookup name (traits ctx M.! typ)) -> do
        let Just ft = lookup name (traits ctx M.! typ)
        return (emptySubst, typeOfArgs ft, expr)

      _ -> lift $ throwE $ NotFound posn v

  Lit      (IntLit    n) -> return (emptySubst, ConType (Id ["int"]), expr)
  Lit      (DoubleLit n) -> return (emptySubst, ConType (Id ["double"]), expr)
  Lit      (CharLit   c) -> return (emptySubst, ConType (Id ["char"]), expr)
  Lit      (StringLit c) -> return (emptySubst, ConType (Id ["string"]), expr)
  Lit      (BoolLit   c) -> return (emptySubst, ConType (Id ["bool"]), expr)
  ArrayLit es            -> do
    b <- fresh
    fmap
        ( \(s, t, u) ->
          (s, AppType (ConType (Id ["array"])) [t], ArrayLit $ reverse u)
        )
      $ foldlM
          ( \(s1, t1, e0) e -> do
            (s2, t2, e') <- algoW e
            s3           <- lift $ mgu t1 t2
            return (s3 `compose` s2 `compose` s1, apply s3 t2, e' : e0)
          )
          (emptySubst, VarType b, [])
          es
  IndexArray arr i -> do
    (s1, t1, arr') <- algoW arr
    b              <- fresh
    s2 <- lift $ mgu t1 (AppType (ConType (Id ["array"])) [VarType b])

    (s3, t3, i')   <- algoW i
    s4             <- lift $ mgu t3 (ConType (Id ["int"]))
    return
      ( s4 `compose` s3 `compose` s2 `compose` s1
      , apply      s2   (VarType b)
      , IndexArray arr' i'
      )
  -- ここでSchemeの引数を無視しているが問題ないか？
  ClosureE (Closure functype@(FuncType _ (ArgType _ _ args) ret) body) -> do
    (s1, t1, body') <- do
      modify $ \ctx -> ctx
        { schemes = foldl' (\mp (s, t) -> M.insert (Id [s]) (Scheme [] t) mp)
                           (schemes ctx)
                           args
        }
      algoW body

    s2 <- lift $ mgu ret t1
    return
      ( s2 `compose` s1
      , apply s2 $ typeOfArgs functype
      , ClosureE (Closure functype body')
      )
  FnCall f es -> do
    (s1, t1, f') <- algoW f
    case t1 of
      FnType args ret -> do
        results <- mapM algoW es
        -- FIXME: ここの長さがあってないときの処理
        let s1 = foldr' compose emptySubst (map (\(x, _, _) -> x) results)
        substs <- lift $ zipWithM mgu args (map (\(_, y, _) -> y) results)
        let s2 = foldr' compose emptySubst substs
        return
          ( s1 `compose` s2
          , apply s2 $ apply s1 ret
          , FnCall f' $ map (\(_, _, z) -> z) results
          )
      _ -> error $ show (FnCall f es, t1)
  Let x expr -> do
    (s1, t1, expr') <- algoW expr
    modify $ \ctx -> apply
      s1
      ( ctx { schemes = M.insert x (generalize (apply s1 ctx) t1) $ schemes ctx
            }
      )
    return (s1, ConType (Id ["unit"]), Let x expr')
  Unit         -> return (emptySubst, ConType (Id ["unit"]), expr)
  Procedure es -> fmap (\(x, y, z) -> (x, y, Procedure $ reverse z)) $ foldlM
    ( \(s1, _, e0) e -> do
      -- discarding the previous type information
      (s2, t2, e') <- algoW e
      return (s2 `compose` s1, t2, e' : e0)
    )
    (emptySubst, ConType (Id ["unit"]), [])
    es
  ForIn elem arr es -> do
    b              <- fresh
    (s1, t1, arr') <- algoW arr
    s2 <- lift $ mgu t1 (AppType (ConType (Id ["array"])) [VarType b])
    ctx            <- get
    modify $ \ctx -> ctx
      { schemes = M.insert (Id [elem]) (Scheme [] (VarType b)) $ schemes ctx
      }
    (s3, t3, es') <- algoW $ Procedure es
    put ctx
    s4 <- lift $ mgu t3 $ ConType (Id ["unit"])
    return
      ( s4 `compose` s3 `compose` s2 `compose` s1
      , ConType (Id ["unit"])
      , ForIn elem arr' $ (\(Procedure e) -> e) es'
      )
  If brs -> do
    b      <- fresh
    substs <- forM brs $ \(cond, expr) -> do
      (s1, t1, cond') <- algoW cond
      s2              <- lift $ mgu t1 (ConType (Id ["bool"]))

      (s3, t3, expr') <- algoW expr
      s4              <- lift $ mgu t3 (VarType b)
      return (s4 `compose` s3 `compose` s2 `compose` s1, (cond', expr'))

    let s = foldr1 compose $ map fst substs
    return (s, apply s (VarType b), If $ map snd substs)
  Op op e1 e2 -> do
    (s1, t1, e1') <- algoW e1
    (s2, t2, e2') <- algoW e2
    case op of
      Eq -> do
        s3 <- lift $ mgu t1 t2
        return
          (s3 `compose` s2 `compose` s1, ConType (Id ["bool"]), Op op e1' e2')
      Leq -> do
        s3 <- lift $ mgu t1 (ConType (Id ["int"]))
        s4 <- lift $ mgu t2 (ConType (Id ["int"]))
        return
          ( s4 `compose` s3 `compose` s2 `compose` s1
          , ConType (Id ["bool"])
          , Op op e1' e2'
          )
  Member e1 v1 -> do
    (s1, t1, e1') <- algoW e1
    case t1 of
      ConType name             -> memberW name v1 s1 t1 e1'
      VarType _                -> lift $ throwE $ CannotInfer e1
      AppType (ConType name) _ -> memberW name v1 s1 t1 e1'
      -- auto dereference
      RefType t                -> algoW $ Member (Deref e1) v1
      _                        -> error $ show t1
  RecordOf name fields -> do
    ctx          <- get
    (tyvars, rc) <- lift $ records ctx M.!? Id [name] ?? NotFound
      Nothing
      (Id ["RecordOf type"])
    substs <- forM fields $ \(field, expr) -> do
      typ             <- lift $ lookup field rc ?? NotFound Nothing (Id [field])
      (s1, t1, expr') <- algoW expr
      s2              <- lift $ mgu t1 typ
      return (s2 `compose` s1, (field, expr'))
    let s = foldr1 compose $ map fst substs

    fvars <- mapM (\_ -> fresh) tyvars
    return
      ( s
      , if null tyvars
        then ConType (Id [name])
        else AppType (ConType (Id [name])) (map VarType fvars)
      , RecordOf name $ map snd substs
      )
  Match e1 brs -> do
    b             <- fresh
    (s1, t1, e1') <- algoW e1

    substs        <- forM brs $ \(pat, expr) -> do
      s2              <- match pat t1
      (s3, t3, expr') <- algoW expr
      s4              <- lift $ mgu t3 (VarType b)
      return $ (s4 `compose` s3 `compose` s2, (pat, expr'))
    let s' = foldl' compose s1 $ map fst substs

    return (s', apply s' (VarType b), Match e1' $ map snd substs)
  Assign e1 e2 -> do
    (s1, t1, e1') <- algoW e1
    t1'           <- case t1 of
      RefType t -> return t
      _         -> lift $ throwE $ CannotAssignToNonReferencedType e1 t1
    (s2, t2, e2') <- algoW e2
    s3            <- lift $ mgu t1' t2

    return (s3 `compose` s2 `compose` s1, ConType (Id ["unit"]), Assign e1' e2')
  Self typ  -> return (emptySubst, typ, Var Nothing (Id ["self"]))
  Stmt expr -> do
    -- ignore t1 here
    (s1, t1, expr') <- algoW expr

    return (s1, ConType (Id ["unit"]), expr')
  Ref v -> do
    (s, t, e) <- algoW v
    return (s, RefType t, Ref e)
  Deref e -> do
    (s, t, e') <- algoW e
    case t of
      RefType t' -> return (s, t', Deref e')
      _          -> lift $ throwE $ CannotDerefNonReferencedType t
  _ -> error $ show expr
 where
  selfTypeToVar typ b = go typ
   where
    go typ = case typ of
      FnType  xs y  -> FnType (map go xs) (go y)
      AppType x  ys -> AppType (go x) (map go ys)
      SelfType      -> VarType b
      RefType t     -> go t
      _             -> typ

  memberW
    :: MonadIO m
    => Id
    -> String
    -> Subst
    -> Type
    -> Expr AlexPosn
    -> StateT
         Context
         (ExceptT TypeCheckExceptions m)
         (Subst, Type, Expr AlexPosn)
  memberW name v1 s1 t1 e1' = do
    StateT $ \ctx -> catchE (runStateT tryRecord ctx) $ \err -> case err of
      NotFound _ _ -> runStateT (tryImpl name) ctx
      _            -> throwE err
   where
    tryRecord = do
      ctx     <- get
      (_, rc) <- lift $ records ctx M.!? name ?? NotFound Nothing name
      t2      <- lift $ lookup v1 rc ?? NotFound Nothing (Id [v1])
      return (s1, t2, Member e1' v1)

    tryImpl
      :: MonadIO m
      => Id
      -> StateT
           Context
           (ExceptT TypeCheckExceptions m)
           (Subst, Type, Expr AlexPosn)
    tryImpl (Id [name']) = do
      ctx        <- get
      traitNames <- lift $ impls ctx M.!? name' ?? NotFound Nothing name
      let functypes = filter (\ft -> fst ft == v1) $ concat $ map
            (traits ctx M.!)
            traitNames
      (methodName, ft) <-
        lift
        $  (\t -> if length t == 1 then Just (head t) else Nothing) functypes
        ?? AmbiguousName v1

      -- SelfTypeを剥がす
      -- ここで、interfaceの型定義はselfを含むのでこれは型変数に書き換える
      -- ついでにRefTypeの処理, 現状はrefできるのはselfのみ
      b <- fresh
      let FnType (arr1:args) ret = selfTypeToVar (typeOfArgs ft) b
      let FuncType _ at _        = ft
      s2 <- lift $ mgu arr1 $ if isRefSelfArgType at then RefType t1 else t1

      return
        ( s2 `compose` s1
        , apply s2 $ FnType args ret
        , MethodOf t1 methodName $ if isRefSelfArgType at then Ref e1' else e1'
        )
    tryImpl name = lift $ throwE $ NotFound Nothing name

  appW (e1:e2:[]) = do
    b             <- VarType <$> fresh
    ctx           <- get
    (s1, t1, e2') <- algoW e2
    put $ apply s1 ctx
    (s2, t2, e1') <- algoW e1
    put ctx
    s3 <- lift $ mgu (apply s2 t1) (FnType [t2] b)
    return (s3 `compose` s2 `compose` s1, apply s3 b, [e1', e2'])
  appW (e1:es) = do
    b             <- VarType <$> fresh
    ctx           <- get
    (s1, t1, es') <- appW es
    put $ apply s1 ctx
    (s2, t2, e1') <- algoW e1
    put ctx
    s3 <- lift $ mgu (apply s2 t1) (FnType [t2] b)
    return (s3 `compose` s2 `compose` s1, apply s3 b, e1' : es')

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
  -> StateT Context (ExceptT TypeCheckExceptions m) (Type, Expr AlexPosn)
typecheckExpr e = fmap (\(_, y, z) -> (y, z)) $ algoW e

typecheckModule
  :: MonadIO m
  => [Decl AlexPosn]
  -> StateT Context (ExceptT TypeCheckExceptions m) [Decl AlexPosn]
typecheckModule ds = mapM check ds
 where
  typeApply []   ty = ty
  typeApply vars ty = AppType ty $ map VarType vars

  check d = case d of
    Enum name tyvars fs -> do
      modify $ \ctx -> ctx
        { schemes = foldl'
          ( \mp (EnumField f typs) -> M.insert
            (Id [name, f])
            ( Scheme tyvars $ if null typs
              then (typeApply tyvars $ ConType (Id [name]))
              else FnType typs (typeApply tyvars $ ConType (Id [name]))
            )
            mp
          )
          (schemes ctx)
          fs
        , enums   = M.insert (Id [name]) (map (\(EnumField f ts) -> (f, ts)) fs)
          $ enums ctx
        }
      return d
    Record r tyvars rds -> do
      modify $ \ctx -> ctx
        { records = M.insert (Id [r])
                             (tyvars, map (\(RecordField s t) -> (s, t)) rds)
          $ records ctx
        }
      return d
    Derive name x typ ds -> do
      case typ of
        Just (ConType (Id [typ'])) -> modify $ \ctx -> ctx
          { impls  = M.insertWith (++) typ' [name] $ impls ctx
          , traits = M.union
              ( M.singleton name
              $ map (\(Func fn (Closure ft _)) -> (fn, ft)) ds
              )
            $ traits ctx
          }
        Nothing -> modify $ \ctx -> ctx
          { impls  = M.insertWith (++) name [name] $ impls ctx
          , traits = M.union
              ( M.singleton name
              $ map (\(Func fn (Closure ft _)) -> (fn, ft)) ds
              )
            $ traits ctx
          }
      ds' <- typecheckModule ds
      return $ Derive name x typ ds'
    OpenD _ -> return d
    Func name c@(Closure argtypes@(FuncType tyvars _ _) _) -> do
      modify $ \ctx -> ctx
        { schemes = M.insert (Id [name]) (Scheme tyvars (typeOfArgs argtypes))
          $ schemes ctx
        }

      ctx      <- get
      (ty, c') <- typecheckExpr (ClosureE c)
      put ctx

      modify
        $ \ctx -> ctx
            { schemes = M.insert (Id [name]) (Scheme tyvars ty) $ schemes ctx
            }

      return $ Func name $ (\(ClosureE f) -> f) c'
    ExternalFunc name (FuncType tyvars arg ret) -> do
      modify $ \ctx -> ctx
        { schemes = M.insert (Id [name])
                             (Scheme tyvars $ FnType (listArgTypes arg) ret)
          $ schemes ctx
        }
      return d
    Interface s _ fs -> do
      modify $ \ctx -> ctx { traits = M.insert s fs $ traits ctx }
      return d

inferTypeE :: MonadIO m => Expr AlexPosn -> ExceptT TypeCheckExceptions m Type
inferTypeE e = fmap fst $ evalStateT (typecheckExpr e) std

runTypeCheckExpr
  :: MonadIO m => Expr AlexPosn -> ExceptT TypeCheckExceptions m (Expr AlexPosn)
runTypeCheckExpr e = fmap snd $ evalStateT (typecheckExpr e) std

runTypeCheckModule
  :: MonadIO m
  => [Decl AlexPosn]
  -> ExceptT TypeCheckExceptions m [Decl AlexPosn]
runTypeCheckModule ds = evalStateT (typecheckModule ds) std
