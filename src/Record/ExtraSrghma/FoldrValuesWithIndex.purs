module Record.ExtraSrghma.FoldrValuesWithIndex where

import Prelude

import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as Record
import Record.ExtraSrghma.MapValuesWithIndex (class MapValuesWithIndex, mapValuesWithIndex)
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

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

foldrValuesWithIndex1
  :: forall row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValuesWithIndex1 rowList row fieldType
  => (String -> fieldType -> fieldType -> fieldType)
  -> Record row
  -> fieldType
foldrValuesWithIndex1 = foldrValuesWithIndexImpl1 @rowList

foldMapValuesWithIndexR1
  :: forall fieldType row rowList row' rowList' m
   . RL.RowToList row rowList
  => RL.RowToList row' rowList'
  => FoldrValuesWithIndex1 rowList' row' m
  => MapValuesWithIndex rowList row fieldType m () row'
  => Monoid m
  => (String -> fieldType -> m)
  -> Record row
  -> m
foldMapValuesWithIndexR1 f r = foldrValuesWithIndex1 (\_ m m2 -> m <> m2) $ mapValuesWithIndex f r

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldrValuesWithIndex1 (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldrValuesWithIndexImpl1
    :: (String -> fieldType -> fieldType -> fieldType)
    -> Record row
    -> fieldType

instance
  ( FoldrValuesWithIndex tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldrValuesWithIndex1 (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldrValuesWithIndexImpl1 f record = foldrValuesWithIndexImpl @tailRowList f accum record
    where
    accum :: fieldType
    accum = Record.get (Proxy :: Proxy name) record
