module Record.ExtraSrghma.Keys.NonEmptyList where

import Data.List (List)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Prim.RowList as RL
import Record.ExtraSrghma.Keys.List (class RLToLK, class RLToLNEK, rlToLK, rlToLNEK)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- NonEmpty List String (NELK)

class RLToNELK (rowList :: RL.RowList Type) where
  rlToNELK :: NonEmpty List String

instance (IsSymbol name, RLToLK tail) => RLToNELK (RL.Cons name ty tail) where
  rlToNELK = reflectSymbol (Proxy :: Proxy name) :| rlToLK @tail

rowToNELK :: forall @row rowList. RL.RowToList row rowList => RLToNELK rowList => NonEmpty List String
rowToNELK = rlToNELK @rowList

class RecordToNELK :: forall k. k -> Constraint
class RecordToNELK a where
  recordToNELK :: NonEmpty List String

instance (RL.RowToList row rowList, RLToNELK rowList) => RecordToNELK (Record row) where
  recordToNELK = rlToNELK @rowList

-- NonEmpty List NonEmptyString (NELNEK)

class RLToNELNEK (rowList :: RL.RowList Type) where
  rlToNELNEK :: NonEmpty List NonEmptyString

instance (IsSymbol name, MakeNonEmpty name, RLToLNEK tail) => RLToNELNEK (RL.Cons name ty tail) where
  rlToNELNEK = nes (Proxy :: Proxy name) :| rlToLNEK @tail

rowToNELNEK :: forall @row rowList. RL.RowToList row rowList => RLToNELNEK rowList => NonEmpty List NonEmptyString
rowToNELNEK = rlToNELNEK @rowList

class RecordToNELNEK :: forall k. k -> Constraint
class RecordToNELNEK a where
  recordToNELNEK :: NonEmpty List NonEmptyString

instance (RL.RowToList row rowList, RLToNELNEK rowList) => RecordToNELNEK (Record row) where
  recordToNELNEK = rlToNELNEK @rowList
