{-# LANGUAGE OverloadedStrings #-}
module Language.Quartz.VM where

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Error
import qualified Data.Map as M
import Data.Dynamic
import Data.Foldable

type Id = String

data Type
  = ConType Id
  | Arrow Type Type
  deriving (Eq, Show)

data Literal
  = IntLit Int
  | DoubleLit Double
  | CharLit Char
  | StringLit String
  deriving (Eq, Show)

data Pattern
  = PVar Id
  | PLit Literal
  | PApp Pattern [Pattern]
  | PAny
  deriving (Eq, Show)

data VM
  = Var Id
  | Lit Literal
  | App VM [VM]
  | Lam [Id] VM
  | Let Id VM VM
  | Case VM Type [(Pattern, VM)]
  | Type Type
  deriving (Eq, Show)

newtype Context = Context { getContext :: M.Map Id VM }

subst :: VM -> Id -> VM -> VM
subst vm var term = case vm of
  Var y | y == var        -> term
  App f xs                -> App f (map (\x -> subst x var term) xs)
  Let  x y1 y2 | x /= var -> Let x (subst y1 var term) (subst y2 var term)
  Case e t  args          -> Case
    (subst e var term)
    t
    ( map
      ( \(pat, br) -> case pat of
        PVar y -> (pat, subst br var term)
      )
      args
    )
  _ -> vm

isNormalForm :: VM -> Bool
isNormalForm vm = case vm of
  Var _            -> True
  Lit _            -> True
  Lam _         _  -> True
  App (Lam _ _) _  -> False
  App f         xs -> isNormalForm f && all isNormalForm xs
  Let  _ _ _       -> False
  Case _ _ _       -> False
  Type _           -> True


match
  :: MonadIO m
  => Pattern
  -> VM
  -> StateT Context (ExceptT RuntimeExceptions m) ()
match pat term = evaluate term >>= \t' -> case (pat, t') of
  (PVar p, t) -> modify (Context . M.insert p t . getContext)
  (PLit lit, Lit lit') ->
    lift $ assertMay (lit == lit') ?? PatternNotMatch (PLit lit) term
  (PApp pf pxs, App f xs) -> match pf f >> mapM_ (uncurry match) (zip pxs xs)
  (PAny       , _       ) -> return ()

data RuntimeExceptions
  = NotFound Id
  | PatternNotMatch Pattern VM
  | PatternExhausted

evaluate :: MonadIO m => VM -> StateT Context (ExceptT RuntimeExceptions m) VM
evaluate vm = case vm of
  _ | isNormalForm vm -> return vm
  Var t -> get >>= \ctx -> lift $ getContext ctx M.!? t ?? NotFound t
  App f xs -> do
    f'  <- evaluate f
    xs' <- mapM evaluate xs

    case f' of
      Lam fargs fbody ->
        let fbody' = foldl' (uncurry . subst) fbody $ zip fargs xs'
        in  evaluate $ if length fargs > length xs'
              then Lam (drop (length xs') fargs) fbody'
              else App fbody' (drop (length fargs) xs')
  Let x t1 t2 -> do
    f <- evaluate t1
    modify (Context . M.insert x f . getContext)
    evaluate t2
  Case t typ brs -> fix
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
