module Record.ExtraSrghma.FoldrValuesLazy where

import Prelude

import Control.Lazy as Lazy
import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as Record
import Record.ExtraSrghma.MapRecord (class MapRecord, mapRecord)
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

foldrValuesLazy
  :: forall accum row fieldType rowList
   . Lazy.Lazy accum
  => RL.RowToList row rowList
  => FoldrValuesLazy rowList row fieldType
  => (fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValuesLazy = foldrValuesLazyImpl @rowList

foldMapValuesLazyR
  :: forall accum row fieldType rowList
   . Lazy.Lazy accum
  => RL.RowToList row rowList
  => FoldrValuesLazy rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesLazyR f = foldrValuesLazy (\x acc -> acc <> f x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rl fieldType
  ) <=
  FoldrValuesLazy rl row fieldType
  | row -> fieldType
  where
  foldrValuesLazyImpl
    :: forall accum
     . Lazy.Lazy accum
    => (fieldType -> accum -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldrValuesLazy tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldrValuesLazy (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldrValuesLazyImpl f accum record = Lazy.defer \_ ->
    f value $ foldrValuesLazyImpl @tailRowList f accum record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record

instance
  Homogeneous row fieldType =>
  FoldrValuesLazy RL.Nil row fieldType
  where
  foldrValuesLazyImpl _ accum _ = accum

-------------------------------------------

-- | Fold over the values of a record, applying the function to each field, but no initial accumulator.
foldrValuesLazy1
  :: forall row fieldType rowList
   . RL.RowToList row rowList
  => Lazy.Lazy fieldType
  => FoldrValuesLazy1 rowList row fieldType
  => (fieldType -> fieldType -> fieldType)
  -> Record row
  -> fieldType
foldrValuesLazy1 = foldrValuesLazyImpl1 @rowList

foldMapValuesLazyR1
  :: forall m row rowList fieldType row' rowList'
   . RL.RowToList row rowList
  => RL.RowToList row' rowList'
  => Lazy.Lazy m
  => FoldrValuesLazy1 rowList' row' m
  => MapRecord rowList row fieldType m () row'
  => Semigroup m
  => (fieldType -> m)
  -> Record row
  -> m
foldMapValuesLazyR1 f r = foldrValuesLazy1 (<>) $ mapRecord f r

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  ) <=
  FoldrValuesLazy1 (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
  foldrValuesLazyImpl1
    :: (fieldType -> fieldType -> fieldType)
    -> Record row
    -> fieldType

instance
  ( FoldrValuesLazy tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , Lazy.Lazy fieldType
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldrValuesLazy1 (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldrValuesLazyImpl1 f record = Lazy.defer \_ ->
    foldrValuesLazyImpl @tailRowList f accum record
    where
    accum :: fieldType
    accum = Record.get (Proxy :: Proxy name) record
