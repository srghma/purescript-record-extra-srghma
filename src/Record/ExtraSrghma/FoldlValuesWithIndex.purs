module Record.ExtraSrghma.FoldlValuesWithIndex where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL

foldlValuesWithIndex
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValuesWithIndex rowList row fieldType
  => (accum -> String -> fieldType -> accum)
  -> accum
  -> Record row
  -> accum
foldlValuesWithIndex = foldlValuesWithIndexImpl @rowList

foldMapValuesWithIndexL
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValuesWithIndex rowList row fieldType
  => Monoid accum
  => (String -> fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesWithIndexL f = foldlValuesWithIndex (\acc key x -> acc <> f key x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldlValuesWithIndex (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldlValuesWithIndexImpl
    :: forall accum
     . (accum -> String -> fieldType -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldlValuesWithIndex tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldlValuesWithIndex (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldlValuesWithIndexImpl f accum record = foldlValuesWithIndexImpl @tailRowList f accum' record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record

    key :: String
    key = reflectSymbol (Proxy :: Proxy name)

    accum' = f accum key value

instance
  Homogeneous row fieldType =>
  FoldlValuesWithIndex RL.Nil row fieldType
  where
  foldlValuesWithIndexImpl _ accum _ = accum

---------------------

---- NOTE: actually there is no need for them bc it is not visible in output types

foldlValuesWithIndex1
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValuesWithIndex1 rowList row fieldType
  => (accum -> String -> fieldType -> accum)
  -> accum
  -> Record row
  -> accum
foldlValuesWithIndex1 = foldlValuesWithIndexImpl1 @rowList

foldMapValuesWithIndexL1
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValuesWithIndex1 rowList row fieldType
  => Monoid accum
  => (String -> fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesWithIndexL1 f = foldlValuesWithIndex1 (\acc key x -> acc <> f key x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldlValuesWithIndex1 (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldlValuesWithIndexImpl1
    :: forall accum
     . (accum -> String -> fieldType -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldlValuesWithIndex1 tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldlValuesWithIndex1 (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldlValuesWithIndexImpl1 f accum record = foldlValuesWithIndexImpl1 @tailRowList f accum' record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record

    key :: String
    key = reflectSymbol (Proxy :: Proxy name)

    accum' = f accum key value
