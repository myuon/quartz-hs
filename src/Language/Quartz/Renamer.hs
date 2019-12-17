module Language.Quartz.Renamer where

import Language.Quartz.AST

varToConType :: [String] -> Type -> Type
varToConType vars t = case t of
  ConType (Id [i]) | i `elem` vars -> VarType i
  ArrowType x y -> ArrowType (varToConType vars x) (varToConType vars y)
  AppType x xs -> AppType (varToConType vars x) (map (varToConType vars) xs)
  SelfType -> VarType "?self"
  _ -> t

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
    NoExpr           -> NoExpr
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

transformVarConTypeD :: Decl posn -> Decl posn
transformVarConTypeD decl = go [] decl
 where
  go vars' decl = case decl of
    Enum name vars efs ->
      Enum name vars (map (goEnumField (vars' ++ vars)) efs)
    Record name vars rfs ->
      Record name vars (map (goRecordField (vars' ++ vars)) rfs)
    OpenD s -> OpenD s
    Func name (Closure args expr) ->
      Func name (Closure (varToConTypeArgTypes vars' args) expr)
    ExternalFunc name args ->
      ExternalFunc name (varToConTypeArgTypes vars' args)
    Trait name vars fs -> Trait name vars (map (goFnType (vars' ++ vars)) fs)
    Instance name vars implFor ds ->
      Instance name vars implFor $ map (go (vars' ++ vars)) ds

  goEnumField vars (EnumField s ts) = EnumField s (map (varToConType vars) ts)
  goRecordField vars (RecordField s t) = RecordField s (varToConType vars t)
  goFnType vars' (FuncType name args) =
    FuncType name (varToConTypeArgTypes vars' args)
