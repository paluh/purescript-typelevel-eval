module Test.Main where

import Prelude

import Data.Generic.Rep (Argument, Product) as GR
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Prim.RowList (Cons, Nil) as RL
import Type.Eval (class Eval, TEProxy(..), proxyEval, kind TypeExpr)
import Type.Eval.Boolean (BProxy, Eq, False, True)
import Type.Eval.Foldable (All, FoldrWithIndex)
import Type.Eval.Function (type (<<<), Const)
import Type.Eval.Functor (Map)
import Type.Eval.Generic (GenericRep)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Eval.ValueOf (ValueOf)
import Type.Eval.ValueOf as ValueOf
import Type.Prelude (RLProxy, SProxy)
import Type.Proxy (Proxy)
import Type.Row (RProxy)

type Test_Map_RowList =
  ToRow <<< Map (Const String) <<< FromRow

test_Map_RowList ::
  (Proxy
    (RProxy (a :: String, b :: String, c :: String)))
test_Map_RowList = proxyEval
  (TEProxy :: TEProxy
    (Test_Map_RowList (RProxy (a :: Int, b :: Boolean, c :: Number))))

type Test_All_RowList =
  All (Eq String) <<< FromRow

test_All_RowList1 ::
  (Proxy
    (BProxy True))
test_All_RowList1 = proxyEval
  (TEProxy :: TEProxy
    (Test_All_RowList (RProxy (a :: String, b :: String, c :: String))))

test_All_RowList2 ::
  (Proxy
    (BProxy False))
test_All_RowList2 = proxyEval
  (TEProxy :: TEProxy
    (Test_All_RowList (RProxy (a :: String, b :: String, c :: Int))))

data E a b c d e = L a b c | M d | R e
derive instance genericE :: Generic (E a b c d e) _

foreign import data RLCons :: Type -> Type -> TypeExpr -> TypeExpr
foreign import data RLNil :: TypeExpr

instance evalRLNil :: Eval RLNil (RLProxy RL.Nil)

instance evalRLCons ::
  ( Eval t (RLProxy t')
  ) => Eval (RLCons (SProxy sym) a t) (RLProxy (RL.Cons sym a t'))

type Test_Generic_RowList =
  ToRow <<< FoldrWithIndex RLCons RLNil <<< GenericRep

test_Generic_RowList :: Proxy
   (RProxy
      ( "L" :: GR.Product
        (GR.Argument Boolean) (GR.Product (GR.Argument String) (GR.Argument Int))
      , "M" :: GR.Argument Number
      , "R" :: GR.Argument (Record ())))
test_Generic_RowList = proxyEval
  (TEProxy :: TEProxy
    (Test_Generic_RowList (E Boolean String Int Number {})))

foreign import data Elem :: Type -> TypeExpr

instance evalElemString :: Eval (Elem String) Char

instance evalElemArray :: Eval (Elem (Array a)) a

testValueOfString :: ValueOf (Elem String)
testValueOfString = ValueOf.from 'a'

testFromValueOfString :: Char
testFromValueOfString = ValueOf.to testValueOfString

testValueOfArray :: ValueOf (Elem (Array Int))
testValueOfArray = ValueOf.from 1

testFromValueOfArray :: Int
testFromValueOfArray = ValueOf.to testValueOfArray

main :: Effect Unit
main = pure unit
