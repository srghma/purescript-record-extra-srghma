module Record.ExtraSrghma.FoldrValuesWithIndex where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL

foldrValuesWithIndex
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValuesWithIndex rowList row fieldType
  => (String -> fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValuesWithIndex = foldrValuesWithIndexImpl @rowList

foldMapValuesWithIndexR
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValuesWithIndex rowList row fieldType
  => Monoid accum
  => (String -> fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesWithIndexR f = foldrValuesWithIndex (\key x acc -> acc <> f key x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldrValuesWithIndex (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldrValuesWithIndexImpl
    :: forall accum
     . (String -> fieldType -> accum -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldrValuesWithIndex tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldrValuesWithIndex (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldrValuesWithIndexImpl f accum record = f key value $ foldrValuesWithIndexImpl @tailRowList f accum record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record

    key :: String
    key = reflectSymbol (Proxy :: Proxy name)

instance
  Homogeneous row fieldType =>
  FoldrValuesWithIndex RL.Nil row fieldType
  where
  foldrValuesWithIndexImpl _ accum _ = accum

---------------------------

---- NOTE: actually there is no need for them bc it is not visible in output types

foldrValuesWithIndex1
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValuesWithIndex1 rowList row fieldType
  => (String -> fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValuesWithIndex1 = foldrValuesWithIndexImpl1 @rowList

foldMapValuesWithIndexR1
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValuesWithIndex1 rowList row fieldType
  => Monoid accum
  => (String -> fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesWithIndexR1 f = foldrValuesWithIndex1 (\key x acc -> acc <> f key x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldrValuesWithIndex1 (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldrValuesWithIndexImpl1
    :: forall accum
     . (String -> fieldType -> accum -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldrValuesWithIndex1 tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldrValuesWithIndex1 (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldrValuesWithIndexImpl1 f accum record = f key value $ foldrValuesWithIndexImpl1 @tailRowList f accum record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record

    key :: String
    key = reflectSymbol (Proxy :: Proxy name)
