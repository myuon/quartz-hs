module Language.Quartz.Std where

import qualified Data.Map as M
import Data.Dynamic
import Language.Quartz.AST

ffi :: M.Map Id ([Dynamic] -> IO Expr)
ffi = M.fromList
  [(Id ["println"], \[d] -> print (fromDynamic d :: Maybe Expr) >> return Unit)]

types :: M.Map Id Type
types = M.fromList
  [(Id ["println"], ConType (Id ["string"]) `ArrowType` ConType (Id ["unit"]))]

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
