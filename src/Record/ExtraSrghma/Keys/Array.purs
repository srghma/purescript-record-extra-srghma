module Record.ExtraSrghma.Keys.Array where

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- Array
----- RL
class RowListToArrayOfKeys (rowList :: RL.RowList Type) where
  rowListToArrayOfKeys :: Array String

instance RowListToArrayOfKeys RL.Nil where
  rowListToArrayOfKeys = []

instance (IsSymbol name, RowListToArrayOfKeys tail) => RowListToArrayOfKeys (RL.Cons name ty tail) where
  rowListToArrayOfKeys = Array.cons (reflectSymbol (Proxy :: Proxy name)) (rowListToArrayOfKeys @tail)

----- Row
rowToArrayOfKeys :: forall @row rowList. RL.RowToList row rowList => RowListToArrayOfKeys rowList => Array String
rowToArrayOfKeys = rowListToArrayOfKeys @rowList

----- Record
class RecordToArrayOfKeys :: forall k. k -> Constraint
class RecordToArrayOfKeys a where
  recordToArrayOfKeys :: Array String

instance (RL.RowToList row rowList, RowListToArrayOfKeys rowList) => RecordToArrayOfKeys (Record row) where
  recordToArrayOfKeys = rowListToArrayOfKeys @rowList

-- NonEmptyArray
----- RL
class RowListToArrayOfKeys1 (rowList :: RL.RowList Type) where
  rowListToArrayOfKeys1 :: NonEmptyArray String

instance (IsSymbol name, RowListToArrayOfKeys tail) => RowListToArrayOfKeys1 (RL.Cons name ty tail) where
  rowListToArrayOfKeys1 = NonEmptyArray.cons' (reflectSymbol (Proxy :: Proxy name)) (rowListToArrayOfKeys @tail)

----- Row
rowToArrayOfKeys1 :: forall @row rowList. RL.RowToList row rowList => RowListToArrayOfKeys1 rowList => NonEmptyArray String
rowToArrayOfKeys1 = rowListToArrayOfKeys1 @rowList

----- Record
class RecordToArrayOfKeys1 :: forall k. k -> Constraint
class RecordToArrayOfKeys1 a where
  recordToArrayOfKeys1 :: NonEmptyArray String

instance (RL.RowToList row rowList, RowListToArrayOfKeys1 rowList) => RecordToArrayOfKeys1 (Record row) where
  recordToArrayOfKeys1 = rowListToArrayOfKeys1 @rowList
