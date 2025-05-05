module Record.ExtraSrghma.Keys.Array where

import Data.Array as Array
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- Array String (AK)

class RLToAK (rowList :: RL.RowList Type) where
  rlToAK :: Array String

instance RLToAK RL.Nil where
  rlToAK = []

instance (IsSymbol name, RLToAK tail) => RLToAK (RL.Cons name ty tail) where
  rlToAK = Array.cons (reflectSymbol (Proxy :: Proxy name)) (rlToAK @tail)

rowToAK :: forall @row rowList. RL.RowToList row rowList => RLToAK rowList => Array String
rowToAK = rlToAK @rowList

class RecordToAK :: forall k. k -> Constraint
class RecordToAK a where
  recordToAK :: Array String

instance (RL.RowToList row rowList, RLToAK rowList) => RecordToAK (Record row) where
  recordToAK = rlToAK @rowList

-- Array NonEmptyString (ANEK)

class RLToANEK (rowList :: RL.RowList Type) where
  rlToANEK :: Array NonEmptyString

instance RLToANEK RL.Nil where
  rlToANEK = []

instance (IsSymbol name, MakeNonEmpty name, RLToANEK tail) => RLToANEK (RL.Cons name ty tail) where
  rlToANEK = Array.cons (nes (Proxy :: Proxy name)) (rlToANEK @tail)

rowToANEK :: forall @row rowList. RL.RowToList row rowList => RLToANEK rowList => Array NonEmptyString
rowToANEK = rlToANEK @rowList

class RecordToANEK :: forall k. k -> Constraint
class RecordToANEK a where
  recordToANEK :: Array NonEmptyString

instance (RL.RowToList row rowList, RLToANEK rowList) => RecordToANEK (Record row) where
  recordToANEK = rlToANEK @rowList
