module Record.ExtraSrghma.Keys.NonEmptyArray where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Prim.RowList as RL
import Record.ExtraSrghma.Keys.Array
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- NonEmptyArray String (NEAK)

class RLToNEAK (rowList :: RL.RowList Type) where
  rlToNEAK :: NonEmptyArray String

instance (IsSymbol name, RLToAK tail) => RLToNEAK (RL.Cons name ty tail) where
  rlToNEAK = NonEmptyArray.cons' (reflectSymbol (Proxy :: Proxy name)) (rlToAK @tail)

rowToNEAK :: forall @row rowList. RL.RowToList row rowList => RLToNEAK rowList => NonEmptyArray String
rowToNEAK = rlToNEAK @rowList

class RecordToNEAK :: forall k. k -> Constraint
class RecordToNEAK a where
  recordToNEAK :: NonEmptyArray String

instance (RL.RowToList row rowList, RLToNEAK rowList) => RecordToNEAK (Record row) where
  recordToNEAK = rlToNEAK @rowList

-- NonEmptyArray NonEmptyString (NEANEK)

class RLToNEANEK (rowList :: RL.RowList Type) where
  rlToNEANEK :: NonEmptyArray NonEmptyString

instance (IsSymbol name, MakeNonEmpty name, RLToANEK tail) => RLToNEANEK (RL.Cons name ty tail) where
  rlToNEANEK = NonEmptyArray.cons' (nes (Proxy :: Proxy name)) (rlToANEK @tail)

rowToNEANEK :: forall @row rowList. RL.RowToList row rowList => RLToNEANEK rowList => NonEmptyArray NonEmptyString
rowToNEANEK = rlToNEANEK @rowList

class RecordToNEANEK :: forall k. k -> Constraint
class RecordToNEANEK a where
  recordToNEANEK :: NonEmptyArray NonEmptyString

instance (RL.RowToList row rowList, RLToNEANEK rowList) => RecordToNEANEK (Record row) where
  recordToNEANEK = rlToNEANEK @rowList
