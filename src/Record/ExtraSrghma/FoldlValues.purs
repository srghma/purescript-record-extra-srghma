module Record.ExtraSrghma.FoldlValues where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL

foldlValues
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValues rowList row fieldType
  => (accum -> fieldType -> accum)
  -> accum
  -> Record row
  -> accum
foldlValues = foldlValuesImpl (Proxy :: Proxy rowList)

foldMapValuesL
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValues rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesL f = foldlValues (\acc x -> acc <> f x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldlValues (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldlValuesImpl
    :: forall accum
     . Proxy rowList
    -> (accum -> fieldType -> accum)
    -> accum
    -> Record row
    -> accum

instance foldlValuesCons ::
  ( FoldlValues tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldlValues (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldlValuesImpl _ f accum record = foldlValuesImpl tailProxy f accum' record
    where
    tailProxy :: Proxy tailRowList
    tailProxy = Proxy

    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record

    accum' = f accum value

instance foldlValuesNil ::
  Homogeneous row fieldType =>
  FoldlValues RL.Nil row fieldType
  where
  foldlValuesImpl _ _ accum _ = accum
