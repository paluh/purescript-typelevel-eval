module Type.Eval.Generic where

import Data.Generic.Rep (class Generic)
import Type.Eval (class Eval, kind TypeExpr)

foreign import data GenericRep :: Type -> TypeExpr

instance evalGenericRep ::
  ( Generic t rep
  ) =>
  Eval (GenericRep t) rep


