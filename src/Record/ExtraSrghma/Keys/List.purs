module Record.ExtraSrghma.Keys.List where

import Prelude
import Data.List (List, (:))
import Data.NonEmpty (NonEmpty, (:|))
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- List
----- RL
class RowListToListOfKeys (rowList :: RL.RowList Type) where
  rowListToListOfKeys :: List String

instance RowListToListOfKeys RL.Nil where
  rowListToListOfKeys = mempty

instance (IsSymbol name, RowListToListOfKeys tail) => RowListToListOfKeys (RL.Cons name ty tail) where
  rowListToListOfKeys = reflectSymbol (Proxy :: Proxy name) : rowListToListOfKeys @tail

----- Row

rowToListOfKeys :: forall @row rowList. RL.RowToList row rowList => RowListToListOfKeys rowList => List String
rowToListOfKeys = rowListToListOfKeys @rowList

----- Record

-- I had to use class, because cannot pass record as VTA `recordKeys @{ bar :: String, baz :: Boolean }` throws error
class RecordToListOfKeys :: forall k. k -> Constraint
class RecordToListOfKeys a where
  recordToListOfKeys :: List String

instance (RL.RowToList row rowList, RowListToListOfKeys rowList) => RecordToListOfKeys (Record row) where
  recordToListOfKeys = rowListToListOfKeys @rowList

-------------------------

-- NEList
----- RL
class RowListToListOfKeys1 (rowList :: RL.RowList Type) where
  rowListToListOfKeys1 :: NonEmpty List String

instance (IsSymbol name, RowListToListOfKeys tail) => RowListToListOfKeys1 (RL.Cons name ty tail) where
  rowListToListOfKeys1 = reflectSymbol (Proxy :: Proxy name) :| rowListToListOfKeys @tail

----- Row
rowToListOfKeys1 :: forall @row rowList. RL.RowToList row rowList => RowListToListOfKeys1 rowList => NonEmpty List String
rowToListOfKeys1 = rowListToListOfKeys1 @rowList

----- Record
class RecordToListOfKeys1 :: forall k. k -> Constraint
class RecordToListOfKeys1 a where
  recordToListOfKeys1 :: NonEmpty List String

instance (RL.RowToList row rowList, RowListToListOfKeys1 rowList) => RecordToListOfKeys1 (Record row) where
  recordToListOfKeys1 = rowListToListOfKeys1 @rowList
