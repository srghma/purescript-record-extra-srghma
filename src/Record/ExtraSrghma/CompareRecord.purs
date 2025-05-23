module Record.ExtraSrghma.CompareRecord where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Prim.Row as Row
import Prim.RowList as RL

-- | Type class to compare two records field by field, following a known ordering of keys (a RowList).
class OrdRecord :: forall k. k -> Row Type -> Constraint
class
  OrdRecord rl row
  | rl -> row
  where
  compareRecordImpl :: Record row -> Record row -> Ordering

-- | Compare two records by the current head field, and recurse on the tail if equal.
instance
  ( IsSymbol name
  , Ord ty
  , Row.Cons name ty trash row
  , OrdRecord tail row
  ) =>
  OrdRecord (RL.Cons name ty tail) row where
  compareRecordImpl a b =
    case compare valA valB of
      EQ -> compareRecordImpl @tail a b
      ordering -> ordering
    where
    namep = Proxy :: Proxy name
    valA = Record.get namep a
    valB = Record.get namep b

-- | Base case: comparing empty RowList always returns EQ.
instance OrdRecord RL.Nil row where
  compareRecordImpl _ _ = EQ

-- | Public interface for comparing two records deterministically by their keys.
-- | You must provide a type with an instance of `RL.RowToList` to determine key ordering.
compareRecord
  :: forall row rl
   . RL.RowToList row rl
  => OrdRecord rl row
  => Record row
  -> Record row
  -> Ordering
compareRecord a b = compareRecordImpl @rl a b
