module Language.Quartz.Renamer where

import Language.Quartz.AST

varToConType :: [String] -> Type -> Type
varToConType vars t = case t of
  ConType (Id [i]) | i `elem` vars -> VarType i
  ArrowType x y -> ArrowType (varToConType vars x) (varToConType vars y)
  AppType x xs -> AppType (varToConType vars x) (map (varToConType vars) xs)
  SelfType -> VarType "?self"
  _ -> t

transformVarConType :: Decl posn -> Decl posn
transformVarConType decl = go [] decl
 where
  go vars' decl = case decl of
    Enum name vars efs ->
      Enum name vars (map (goEnumField (vars' ++ vars)) efs)
    Record name vars rfs ->
      Record name vars (map (goRecordField (vars' ++ vars)) rfs)
    Func name (Closure args expr) ->
      Func name (Closure (goFnArgs vars' args) expr)
    ExternalFunc name args -> ExternalFunc name (goFnArgs vars' args)
    Trait name vars fs -> Trait name vars (map (goFnType (vars' ++ vars)) fs)
    Instance name vars implFor ds ->
      Instance name vars implFor $ map (go (vars' ++ vars)) ds

  goEnumField vars (EnumField s ts) = EnumField s (map (varToConType vars) ts)
  goRecordField vars (RecordField s t) = RecordField s (varToConType vars t)
  goFnArgs vars' (ArgTypes vars args ret) = ArgTypes
    vars
    (map (\(x, y) -> (x, varToConType (vars' ++ vars) y)) args)
    (varToConType (vars' ++ vars) ret)
  goFnType vars' (FuncType name args) = FuncType name (goFnArgs vars' args)
