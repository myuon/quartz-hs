{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Quartz.Eval where

import           Control.Monad.State
import           Control.Error
import qualified Data.Map                      as M
import           Data.Dynamic
import           Data.Foldable
import           Language.Quartz.AST
import           Language.Quartz.Lexer                    ( AlexPosn(..) )
import           Language.Quartz.TypeCheck                ( fresh )
import qualified Language.Quartz.Std           as Std
import qualified Data.PathTree                 as PathTree
import qualified Data.Primitive.Array          as Array

data Context m = Context {
  decls :: PathTree.PathTree String (Decl AlexPosn),
  ffi :: M.Map Id ([Dynamic] -> ExceptT Std.FFIExceptions m (Expr AlexPosn)),
  impls :: M.Map (String, String) (Expr AlexPosn),
  -- referenceのstoreにも使うので登録したものを動かしてはいけない
  exprs :: M.Map Id (Expr AlexPosn)
}

subst :: Expr AlexPosn -> String -> Expr AlexPosn -> Expr AlexPosn
subst expr var term = case unwrapExpr expr of
  Var y | y == Id [var] -> term
  Var _                 -> expr
  Lit _                 -> expr
  FnCall f xs           -> srcSpanExpr' expr
    $ FnCall (subst f var term) (map (\x -> subst x var term) xs)
  Let x t -> srcSpanExpr' expr $ Let x (subst t var term)
  -- FIXME: shadowingしてる場合
  ClosureE (Closure argtypes e) ->
    srcSpanExpr' expr $ ClosureE (Closure argtypes (subst e var term))
  OpenE _      -> expr
  Match e args -> srcSpanExpr' expr $ Match
    (subst e var term)
    (map (\(pat, br) -> (pat, subst br var term)) args)
  If args -> srcSpanExpr' expr
    $ If (map (\(pat, br) -> (subst pat var term, subst br var term)) args)
  Procedure es ->
    srcSpanExpr' expr $ Procedure $ map (\y -> subst y var term) es
  Unit        -> expr
  FFI p es    -> srcSpanExpr' expr $ FFI p $ map (\x -> subst x var term) es
  Array    _  -> expr
  ArrayLit xs -> srcSpanExpr' expr $ ArrayLit (map (\x -> subst x var term) xs)
  IndexArray e1 e2 ->
    srcSpanExpr' expr $ IndexArray (subst e1 var term) (subst e2 var term)
  ForIn v e es -> srcSpanExpr' expr
    $ ForIn v (subst e var term) (map (\y -> subst y var term) es)
  Op op e1 e2 ->
    srcSpanExpr' expr $ Op op (subst e1 var term) (subst e2 var term)
  Member e1 s -> srcSpanExpr' expr $ Member (subst e1 var term) s
  RecordOf s xs ->
    srcSpanExpr' expr $ RecordOf s (map (\(x, y) -> (x, subst y var term)) xs)
  EnumOf con ts ->
    srcSpanExpr' expr $ EnumOf con (map (\x -> subst x var term) ts)
  Assign e1 e2 ->
    srcSpanExpr' expr $ Assign (subst e1 var term) (subst e2 var term)
  Self _         -> expr
  MethodOf t s e -> srcSpanExpr' expr $ MethodOf t s (subst e var term)
  Any  _         -> expr
  Stmt s         -> srcSpanExpr' expr $ Stmt $ subst s var term
  LetRef x t     -> srcSpanExpr' expr $ LetRef x (subst t var term)
  Deref e        -> srcSpanExpr' expr $ Deref $ subst e var term
  RefTo r        -> srcSpanExpr' expr $ RefTo r

isNormalForm :: Expr AlexPosn -> Bool
isNormalForm vm = case unwrapExpr vm of
  Lit      _    -> True
  ClosureE _    -> True
  OpenE    _    -> True
  Unit          -> True
  Array _       -> True
  RecordOf _ fs -> all isNormalForm $ map snd fs
  EnumOf   _ _  -> True
  Any   _       -> True
  RefTo _       -> True
  _             -> False


match
  :: MonadIO m
  => Pattern
  -> Expr AlexPosn
  -> StateT (Context m) (ExceptT RuntimeExceptions m) ()
match pat term = case (pat, unwrapExpr term) of
  (PVar (Id [v]), t) -> modify $ \ctx ->
    ctx { exprs = M.insert (Id [v]) (srcSpanExpr' term t) (exprs ctx) }
  (PLit lit, Lit lit') ->
    lift $ assertMay (lit == lit') ?? PatternNotMatch (PLit lit) term
  (PApp pf pxs, FnCall f xs) ->
    match pf f >> mapM_ (uncurry match) (zip pxs xs)
  (PVar u, Var v) | u == v       -> return ()
  (PVar u, EnumOf v []) | u == v -> return ()
  (PApp p qs, EnumOf e fs) ->
    match p (srcSpanExpr' term $ Var e) >> zipWithM_ match qs fs
  (PAny, _) -> return ()
  _         -> lift $ throwE $ PatternNotMatch pat term

data RuntimeExceptions
  = NotFound (Maybe AlexPosn) Id
  | PatternNotMatch Pattern (Expr AlexPosn)
  | PatternExhausted
  | FFIExceptions Std.FFIExceptions
  | Unreachable (Expr AlexPosn)
  | NumberOfArgumentsDoesNotMatch (Expr AlexPosn)
  deriving Show

-- Assume renaming is done
evalE
  :: MonadIO m
  => Expr AlexPosn
  -> StateT (Context m) (ExceptT RuntimeExceptions m) (Expr AlexPosn)
evalE vm = case unwrapExpr vm of
  _ | isNormalForm vm -> return vm
  Var t               -> do
    ctx <- get
    case t of
      _ | t `M.member` exprs ctx -> do
        return $ exprs ctx M.! t

      Id [typ, name] | (name, typ) `M.member` impls ctx -> do
        return $ impls ctx M.! (name, typ)

      _ -> lift $ throwE $ NotFound (Just $ fst $ getSrcSpan vm) t

  -- tricky part!
  -- トレイとのメソッド呼び出しx.f(y)はT::f(x,y)と解釈し直すが、このsyntaxはFnCallが外側に来てしまっているので
  -- 1段ネストの深いパターンマッチが必要
  FnCall (ExprLoc p q (MethodOf typ name e1)) es -> do
    ctx     <- get
    typName <- lift $ nameOfType typ ?? Unreachable vm
    expr    <- lift $ impls ctx M.!? (name, typName) ?? NotFound Nothing
                                                                 (Id [name])
    evalE $ ExprLoc p q $ FnCall expr (e1 : es)

  FnCall f xs -> do
    f'  <- evalE f
    xs' <- mapM evalE xs
    case unwrapExpr f' of
      ClosureE (Closure (FuncType _ fargs _) fbody)
        | length (listArgTypes fargs) == length xs -> do
          let fbody' =
                foldl' (uncurry . subst) fbody $ zip (listArgNames fargs) xs'
          evalE fbody'
      _ -> lift $ throwE $ NumberOfArgumentsDoesNotMatch vm
  Let x t -> do
    f <- evalE t
    modify $ \ctx -> ctx { exprs = M.insert x f (exprs ctx) }
    return $ srcSpanExpr' vm $ Unit
  Match t brs0 -> do
    t' <- evalE t
    fix
      (\cont brs -> case brs of
        []              -> lift $ throwE PatternExhausted
        ((pat, b) : bs) -> do
          ctx0   <- get
          result <- lift $ lift $ runExceptT $ execStateT (match pat t') ctx0
          case result of
            Left  _    -> cont bs
            Right ctx' -> put ctx' >> evalE b
      )
      brs0
  Procedure es ->
    foldl' (\m e -> m >> evalE e) (return $ srcSpanExpr' vm Unit) es
  FFI p es -> get >>= \ctx -> do
    pf <- lift $ ffi ctx M.!? p ?? NotFound Nothing p
    lift $ withExceptT FFIExceptions $ pf $ map toDyn es
  ArrayLit es -> do
    es' <- mapM evalE es
    let arr = Array.fromList es'
    marr <- liftIO $ Array.thawArray arr 0 (Array.sizeofArray arr)
    return $ srcSpanExpr' vm $ Array $ MArray marr
  IndexArray e1 e2 -> do
    arr <- evalE e1
    ie  <- evalE e2
    case (unwrapExpr arr, unwrapExpr ie) of
      (Array m, Lit (IntLit i)) -> liftIO $ Array.readArray (getMArray m) i
      _                         -> lift $ throwE $ Unreachable vm
  ForIn var e1 es -> do
    arr  <- evalE e1
    ctx0 <- get
    case unwrapExpr arr of
      (Array m) ->
        forM_ [0 .. Array.sizeofMutableArray (getMArray m) - 1] $ \i -> do
          r <- liftIO $ Array.readArray (getMArray m) i
          modify $ \ctx -> ctx { exprs = M.insert (Id [var]) r (exprs ctx) }
          mapM_ evalE es
      _ -> lift $ throwE $ Unreachable vm
    put ctx0

    return $ srcSpanExpr' vm Unit
  If brs0 -> fix
    (\cont brs -> case brs of
      []                    -> return $ srcSpanExpr' vm Unit
      ((cond, br) : others) -> do
        result <- evalE cond
        case unwrapExpr result of
          Lit (BoolLit True ) -> evalE br
          Lit (BoolLit False) -> cont others
          _                   -> error $ show result
    )
    brs0
  Op op e1 e2 -> do
    r1 <- evalE e1
    r2 <- evalE e2
    case (op, unwrapExpr r1, unwrapExpr r2) of
      (Eq, _, _) -> return $ srcSpanExpr r1 r2 $ if r1 == r2
        then Lit (BoolLit True)
        else Lit (BoolLit False)
      (Leq, Lit (IntLit x), Lit (IntLit y)) ->
        return $ srcSpanExpr r1 r2 $ if x <= y
          then Lit (BoolLit True)
          else Lit (BoolLit False)
      _ -> lift $ throwE $ Unreachable vm
  Member e1 v1 -> do
    r1 <- evalE e1
    case unwrapExpr r1 of
      RecordOf _ fields -> do
        return $ (\(Just x) -> x) $ lookup v1 fields
      -- auto dereference
      _ -> lift $ throwE $ Unreachable vm
  Stmt e -> evalE e
  RecordOf name fs ->
    fmap (srcSpanExpr' vm . RecordOf name) $ forM fs $ \(x, y) -> do
      y' <- evalE y
      return (x, y')
  Assign e1 e2 -> do
    case unwrapExpr e1 of
      -- r[i] = e;の形は特別扱い
      IndexArray arr i -> do
        arr' <- evalE arr
        i'   <- evalE i
        r    <- evalE e2
        case (unwrapExpr arr', unwrapExpr i') of
          (RefTo ref, Lit (IntLit n)) -> do
            ctx <- get
            let Array marr = unwrapExpr $ exprs ctx M.! ref
            liftIO $ Array.writeArray (getMArray marr) n r
      -- r.x = e;の形は特別扱い
      Member r v -> do
        r1 <- evalE r
        r2 <- evalE e2
        case unwrapExpr r1 of
          RefTo ref -> do
            modify $ \ctx -> ctx
              { exprs =
                M.adjust
                    (\(ExprLoc p q (RecordOf name rc)) ->
                      ExprLoc p q
                        $ RecordOf name
                        $ fmap (\(x, y) -> if x == v then (x, r2) else (x, y))
                        $ rc
                    )
                    ref
                  $ exprs ctx
              }
      _ -> do
        r1 <- evalE e1
        r2 <- evalE e2
        case unwrapExpr r1 of
          RefTo ref -> do
            modify $ \ctx -> ctx { exprs = M.insert ref r2 $ exprs ctx }
    return $ srcSpanExpr' vm $ Unit
  LetRef x e -> do
    -- reference typeとしてallocateされたものはGCがないので解放されない
    b <- fresh
    let key = Id ["?ref{" ++ show b ++ "}"]

    v <- evalE e
    modify $ \ctx -> ctx
      { exprs = M.insert key v $ M.insert (Id [x]) (srcSpanExpr' vm $ RefTo key) $ exprs ctx
      }
    return $ srcSpanExpr' vm $ RefTo key
  Deref e -> do
    r <- evalE e
    case unwrapExpr r of
      RefTo ref -> do
        ctx <- get
        return $ exprs ctx M.! ref
      _ -> lift $ throwE $ Unreachable vm
  _ -> lift $ throwE $ Unreachable vm

runEvalE
  :: MonadIO m => Expr AlexPosn -> ExceptT RuntimeExceptions m (Expr AlexPosn)
runEvalE m = evalStateT (evalE m) (std M.empty)

evalD
  :: MonadIO m
  => Decl AlexPosn
  -> StateT (Context m) (ExceptT RuntimeExceptions m) ()
evalD decl = go [] decl
 where
  go _ d = case d of
    Enum name _ fs -> do
      forM_ fs $ \(EnumField f typs) -> do
        bs <- mapM (\_ -> fresh) typs
        let vars = map
              (ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) . Var . Id . return)
              bs
        modify $ \ctx -> ctx
          { exprs =
            M.insert
                (Id [name, f])
                (ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) $ if null typs
                  then EnumOf (Id [name, f]) []
                  else ClosureE
                    (Closure
                      (FuncType [] (ArgType False False $ zip bs typs) NoType)
                      (ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) $ EnumOf (Id [name, f]) vars)
                    )
                )
              $ exprs ctx
          }

    Record name _ _ ->
      modify $ \ctx -> ctx { decls = PathTree.insert [name] decl (decls ctx) }
    Func name body -> modify $ \ctx -> ctx
      { exprs = M.insert
                  (Id [name])
                  (ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) $ ClosureE body)
                  (exprs ctx)
      }
    ExternalFunc name (FuncType tyvars args ret) -> do
      bs <- mapM (\_ -> fresh) $ (\(ArgType _ _ xs) -> xs) args
      let args' = zip bs $ listArgTypes args

      evalD $ Func
        name
        (Closure
          (FuncType tyvars (ArgType False False args') ret)
          (ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) $ FFI
            (Id [name])
            ( map (\n -> ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) $ Var (Id [n]))
            $ map fst args'
            )
          )
        )
    Interface _ _ _          -> return ()
    Derive _ _ (Just typ) ds -> do
      let Just typName = nameOfType typ
      modify $ \ctx -> ctx
        { impls = M.union
                    (M.fromList $ map
                      (\(Func fn body) ->
                        ( (fn, typName)
                        , ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) $ ClosureE body
                        )
                      )
                      ds
                    )
                    (impls ctx)
        }
    Derive name _ Nothing ds -> modify $ \ctx -> ctx
      { impls = M.union
                  (M.fromList $ map
                    (\(Func fn body) ->
                      ( (fn, name)
                      , ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) $ ClosureE body
                      )
                    )
                    ds
                  )
                  (impls ctx)
      }
    _ -> error $ show decl

std
  :: MonadIO m
  => M.Map Id ([Dynamic] -> ExceptT Std.FFIExceptions m (Expr AlexPosn))
  -> Context m
std exts = Context { ffi   = M.union Std.ffi exts
                   , exprs = M.empty
                   , decls = PathTree.empty
                   , impls = M.empty
                   }

runMain
  :: MonadIO m => [Decl AlexPosn] -> ExceptT RuntimeExceptions m (Expr AlexPosn)
runMain = runMainWith M.empty

runMainWith
  :: MonadIO m
  => M.Map Id ([Dynamic] -> ExceptT Std.FFIExceptions m (Expr AlexPosn))
  -> [Decl AlexPosn]
  -> ExceptT RuntimeExceptions m (Expr AlexPosn)
runMainWith lib ds = flip evalStateT (std lib) $ do
  mapM_ evalD ds
  evalE
    ( ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0)
    $ FnCall (ExprLoc (AlexPn 0 0 0) (AlexPn 0 0 0) (Var (Id ["main"]))) []
    )
