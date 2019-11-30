module Language.Quartz.Std where

import Control.Monad.IO.Class
import Control.Error
import qualified Data.Map as M
import Data.Dynamic
import Language.Quartz.AST

data FFIExceptions
  = InvalidExpr Dynamic
  deriving Show

ffi :: M.Map Id ([Dynamic] -> ExceptT FFIExceptions IO Expr)
ffi = M.fromList
  [ ( Id ["println"]
    , \[d] -> do
      expr <- (fromDynamic d :: Maybe Expr) ?? InvalidExpr d
      case expr of
        Lit (StringLit s) -> liftIO $ putStrLn s
        Lit (IntLit    n) -> liftIO $ print n
        _                 -> throwE $ InvalidExpr d

      return Unit
    )
  ]

types :: M.Map Id Scheme
types = M.fromList
  [ ( Id ["println"]
    , Scheme ["a"] (VarType "a" `ArrowType` ConType (Id ["unit"]))
    )
  ]

exprs :: M.Map Id Expr
exprs = M.fromList
  [ ( Id ["println"]
    , ClosureE
      ( Closure (ConType (Id ["string"]) `ArrowType` ConType (Id ["unit"]))
                ["x"]
                (FFI (Id ["println"]) [Var (Id ["x"])])
      )
    )
  ]
