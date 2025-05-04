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
foldlValues = foldlValuesImpl @rowList

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
     . (accum -> fieldType -> accum)
    -> accum
    -> Record row
    -> accum

instance
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
  foldlValuesImpl f accum record = foldlValuesImpl @tailRowList f accum' record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record
    accum' = f accum value

instance
  Homogeneous row fieldType =>
  FoldlValues RL.Nil row fieldType
  where
  foldlValuesImpl _ accum _ = accum

----------------

---- NOTE: actually there is no need for them bc it is not visible in output types

foldlValues1
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValues1 rowList row fieldType
  => (accum -> fieldType -> accum)
  -> accum
  -> Record row
  -> accum
foldlValues1 = foldlValuesImpl1 @rowList

foldMapValuesL1
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValues1 rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesL1 f = foldlValues1 (\acc x -> acc <> f x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldlValues1 (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldlValuesImpl1
    :: forall accum
     . (accum -> fieldType -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldlValues1 tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldlValues1 (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldlValuesImpl1 f accum record = foldlValuesImpl1 @tailRowList f accum' record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record
    accum' = f accum value
