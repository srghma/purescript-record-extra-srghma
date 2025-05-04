module Record.ExtraSrghma.FoldrValues where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL

foldrValues
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValues rowList row fieldType
  => (fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValues = foldrValuesImpl @rowList

foldMapValuesR
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValues rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesR f = foldrValues (\x acc -> acc <> f x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldrValues (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldrValuesImpl
    :: forall accum
     . (fieldType -> accum -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldrValues tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldrValues (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldrValuesImpl f accum record = f value $ foldrValuesImpl @tailRowList f accum record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record

instance
  Homogeneous row fieldType =>
  FoldrValues RL.Nil row fieldType
  where
  foldrValuesImpl _ accum _ = accum

-------------------------

---- NOTE: actually there is no need for them bc it is not visible in output types

-- | Fold over the values of a record, applying the function to each field, but no initial accumulator.
foldrValues1
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValues1 rowList row fieldType
  => (fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValues1 = foldrValuesImpl1 @rowList

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldrValues1 (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldrValuesImpl1
    :: forall accum
     . (fieldType -> accum -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldrValues tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldrValues1 (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldrValuesImpl1 f accum record = f value $ foldrValuesImpl @tailRowList f accum record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record

foldMapValuesR1
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValues1 rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesR1 f = foldrValues1 (\x acc -> acc <> f x) mempty
