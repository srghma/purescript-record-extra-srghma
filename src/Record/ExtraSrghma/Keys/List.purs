module Record.ExtraSrghma.Keys.List where

import Data.List (List(..), (:))
import Prim.RowList as RL
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- List String (LK)

class RLToLK (rowList :: RL.RowList Type) where
  rlToLK :: List String

instance RLToLK RL.Nil where
  rlToLK = Nil

instance (IsSymbol name, RLToLK tail) => RLToLK (RL.Cons name ty tail) where
  rlToLK = reflectSymbol (Proxy :: Proxy name) : rlToLK @tail

rowToLK :: forall @row rowList. RL.RowToList row rowList => RLToLK rowList => List String
rowToLK = rlToLK @rowList

-- I had to use class, because cannot pass record as VTA `recordKeys @{ bar :: String, baz :: Boolean }` throws error
class RecordToLK :: forall k. k -> Constraint
class RecordToLK a where
  recordToLK :: List String

instance (RL.RowToList row rowList, RLToLK rowList) => RecordToLK (Record row) where
  recordToLK = rlToLK @rowList

-- List NonEmptyString (LNEK)

class RLToLNEK (rowList :: RL.RowList Type) where
  rlToLNEK :: List NonEmptyString

instance RLToLNEK RL.Nil where
  rlToLNEK = Nil

instance (IsSymbol name, MakeNonEmpty name, RLToLNEK tail) => RLToLNEK (RL.Cons name ty tail) where
  rlToLNEK = nes (Proxy :: Proxy name) : rlToLNEK @tail

rowToLNEK :: forall @row rowList. RL.RowToList row rowList => RLToLNEK rowList => List NonEmptyString
rowToLNEK = rlToLNEK @rowList

class RecordToLNEK :: forall k. k -> Constraint
class RecordToLNEK a where
  recordToLNEK :: List NonEmptyString

instance (RL.RowToList row rowList, RLToLNEK rowList) => RecordToLNEK (Record row) where
  recordToLNEK = rlToLNEK @rowList
