module Record.ExtraSrghma.FoldlValues where

import Prelude

import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as Record
import Record.ExtraSrghma.MapRecord (class MapRecord, mapRecord)
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

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
  ( HomogeneousRowList rowList fieldType
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

foldlValues1
  :: forall row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValues1 rowList row fieldType
  => (fieldType -> fieldType -> fieldType)
  -> Record row
  -> fieldType
foldlValues1 = foldlValuesImpl1 @rowList

foldMapValuesL1
  :: forall m row rowList fieldType row' rowList'
   . RL.RowToList row rowList
  => RL.RowToList row' rowList'
  => FoldlValues1 rowList' row' m
  => MapRecord rowList row fieldType m () row'
  => Semigroup m
  => (fieldType -> m)
  -> Record row
  -> m
foldMapValuesL1 f r = foldlValues1 (<>) $ mapRecord f r

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldlValues1 (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldlValuesImpl1
    :: (fieldType -> fieldType -> fieldType)
    -> Record row
    -> fieldType

instance
  ( FoldlValues tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldlValues1 (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldlValuesImpl1 f record = foldlValuesImpl @tailRowList f accum record
    where
    accum :: fieldType
    accum = Record.get (Proxy :: Proxy name) record
