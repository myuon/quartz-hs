module Language.Quartz.Transform where

import Language.Quartz.AST

varToConType :: [String] -> Type -> Type
varToConType vars t = case t of
  ConType (Id [i]) | i `elem` vars -> VarType i
  FnType  xs y  -> FnType (map (varToConType vars) xs) (varToConType vars y)
  AppType x  ys -> AppType (varToConType vars x) (map (varToConType vars) ys)
  _             -> t

varToConTypeArgTypes :: [String] -> ArgTypes -> ArgTypes
varToConTypeArgTypes vars' (ArgTypes vars args ret) = ArgTypes
  vars
  (map (\(x, y) -> (x, varToConType (vars' ++ vars) y)) args)
  (varToConType (vars' ++ vars) ret)

transformVarConTypeE :: Expr posn -> Expr posn
transformVarConTypeE expr = go [] expr
 where
  go vars' expr = case expr of
    Var _ _     -> expr
    Lit _       -> expr
    FnCall x ys -> FnCall x (map (go vars') ys)
    Let    x e  -> Let x (go vars' e)
    ClosureE (Closure args e) ->
      ClosureE (Closure (varToConTypeArgTypes vars' args) (go vars' e))
    Match e bs       -> Match (go vars' e) (map (\(p, e) -> (p, go vars' e)) bs)
    If        es     -> If (map (\(x, y) -> (go vars' x, go vars' y)) es)
    Procedure es     -> Procedure (map (go vars') es)
    Unit             -> Unit
    FFI x es         -> FFI x (map (go vars') es)
    Array    _       -> expr
    ArrayLit es      -> ArrayLit (map (go vars') es)
    IndexArray e1 e2 -> IndexArray (go vars' e1) (go vars' e2)
    ForIn s  e  es   -> ForIn s (go vars' e) (map (go vars') es)
    Op    op e1 e2   -> Op op (go vars' e1) (go vars' e2)
    Member   e  r    -> Member (go vars' e) r
    RecordOf s  es   -> RecordOf s (map (\(x, y) -> (x, go vars' y)) es)
    EnumOf   s  es   -> EnumOf s (map (go vars') es)
    Assign   e1 e2   -> Assign (go vars' e1) (go vars' e2)
    Self s           -> Self s

transformVarConTypeD :: Decl posn -> Decl posn
transformVarConTypeD decl = go [] decl
 where
  go vars' decl = case decl of
    Enum name vars efs ->
      Enum name vars (map (goEnumField (vars' ++ vars)) efs)
    Record name vars rfs ->
      Record name vars (map (goRecordField (vars' ++ vars)) rfs)
    OpenD s                       -> OpenD s
    Func name (Closure args expr) -> Func
      name
      (Closure (varToConTypeArgTypes vars' args) (transformVarConTypeE expr))
    ExternalFunc name args ->
      ExternalFunc name (varToConTypeArgTypes vars' args)
    Interface name vars fs ->
      Interface name vars (map (goFnType (vars' ++ vars)) fs)
    Derive name vars implFor ds ->
      Derive name vars implFor $ map (go (vars' ++ vars)) ds

  goEnumField vars (EnumField s ts) = EnumField s (map (varToConType vars) ts)
  goRecordField vars (RecordField s t) = RecordField s (varToConType vars t)
  goFnType vars' (FuncType name args) =
    FuncType name (varToConTypeArgTypes vars' args)

transformSelfTypeE :: Type -> Expr posn -> Expr posn
transformSelfTypeE typ expr = go expr
 where
  apply t typ = case typ of
    SelfType         -> t
    FnType  args ret -> FnType (map (apply t) args) (apply t ret)
    AppType t1   ts  -> AppType (apply t t1) (map (apply t) ts)
    _                -> typ

  go expr = case expr of
    Var _ _                   -> expr
    Lit _                     -> expr
    FnCall x ys               -> FnCall x (map go ys)
    Let    x e                -> Let x (go e)
    ClosureE (Closure args e) -> ClosureE (Closure (goArgTypes args) (go e))
    Match e bs                -> Match (go e) (map (\(p, e) -> (p, go e)) bs)
    If        es              -> If (map (\(x, y) -> (go x, go y)) es)
    Procedure es              -> Procedure (map go es)
    Unit                      -> Unit
    FFI x es                  -> FFI x (map go es)
    Array    _                -> expr
    ArrayLit es               -> ArrayLit (map go es)
    IndexArray e1 e2          -> IndexArray (go e1) (go e2)
    ForIn s  e  es            -> ForIn s (go e) (map go es)
    Op    op e1 e2            -> Op op (go e1) (go e2)
    Member   e  r             -> Member (go e) r
    RecordOf s  es            -> RecordOf s (map (\(x, y) -> (x, go y)) es)
    EnumOf   s  es            -> EnumOf s (map go es)
    Assign   e1 e2            -> Assign (go e1) (go e2)
    Self selfType             -> Self (apply typ selfType)

  goArgTypes (ArgTypes vars args ret) =
    ArgTypes vars (map (\(x, y) -> (x, apply typ y)) args) (apply typ ret)

transformSelfTypeD :: Decl posn -> Decl posn
transformSelfTypeD decl = case decl of
  Derive name vars (Just t) decls ->
    Derive name vars (Just t) $ map (go t) decls
  _ -> decl
 where
  apply t typ = case typ of
    SelfType         -> t
    FnType  args ret -> FnType (map (apply t) args) (apply t ret)
    AppType t1   ts  -> AppType (apply t t1) (map (apply t) ts)
    _                -> typ

  goArgTypes t (ArgTypes vars args ret) =
    ArgTypes vars (map (\(x, y) -> (x, apply t y)) args) (apply t ret)

  go t (Func name (Closure argtypes body)) =
    Func name (Closure (goArgTypes t argtypes) (transformSelfTypeE t body))
