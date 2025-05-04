module Record.ExtraSrghma.FoldrValuesLazy where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL
import Control.Lazy as Lazy

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

---- NOTE: actually there is no need for them bc it is not visible in output types

foldrValuesLazy1
  :: forall accum row fieldType rowList
   . Lazy.Lazy accum
  => RL.RowToList row rowList
  => FoldrValuesLazy1 rowList row fieldType
  => (fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValuesLazy1 = foldrValuesLazyImpl1 @rowList

foldMapValuesLazyR1
  :: forall accum row fieldType rowList
   . Lazy.Lazy accum
  => RL.RowToList row rowList
  => FoldrValuesLazy1 rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesLazyR1 f = foldrValuesLazy1 (\x acc -> acc <> f x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rl fieldType
  ) <=
  FoldrValuesLazy1 rl row fieldType
  | row -> fieldType
  where
  foldrValuesLazyImpl1
    :: forall accum
     . Lazy.Lazy accum
    => (fieldType -> accum -> accum)
    -> accum
    -> Record row
    -> accum

instance
  ( FoldrValuesLazy1 tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) =>
  FoldrValuesLazy1 (RL.Cons name fieldType tailRowList) row fieldType
  where
  foldrValuesLazyImpl1 f accum record = Lazy.defer \_ ->
    f value $ foldrValuesLazyImpl1 @tailRowList f accum record
    where
    value :: fieldType
    value = Record.get (Proxy :: Proxy name) record
