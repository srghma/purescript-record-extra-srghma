module Record.ExtraSrghma.Entries.NonEmptyArray where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String.NonEmpty (NonEmptyString, class MakeNonEmpty, nes)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Data.Tuple (Tuple(..))
import Type.Prelude (class IsSymbol, reflectSymbol)
import Record.ExtraSrghma.Entries.Array (class RLToAKV, class RLToANEKV, rlToAKV, rlToANEKV)

-- NEAKV - NonEmptyArray (Tuple String a)

class
  RLToNEAKV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToNEAKV :: Record row -> NonEmptyArray (Tuple String fieldType)

instance
  ( IsSymbol name
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToAKV tailRowList row fieldType
  ) =>
  RLToNEAKV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToNEAKV record =
    NEA.cons' (Tuple name (Record.get (Proxy :: Proxy name) record)) (rlToAKV @tailRowList record)
    where
    name = reflectSymbol (Proxy :: Proxy name)

recordToNEAKV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToNEAKV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> NonEmptyArray (Tuple String fieldType)
recordToNEAKV = rlToNEAKV @rowList

-- NEANEKV - NonEmptyArray (Tuple NonEmptyString a)

class
  RLToNEANEKV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToNEANEKV :: Record row -> NonEmptyArray (Tuple NonEmptyString fieldType)

instance
  ( IsSymbol name
  , MakeNonEmpty name
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToANEKV tailRowList row fieldType
  ) =>
  RLToNEANEKV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToNEANEKV record =
    NEA.cons' (Tuple (nes (Proxy :: Proxy name)) (Record.get (Proxy :: Proxy name) record)) (rlToANEKV @tailRowList record)

recordToNEANEKV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToNEANEKV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> NonEmptyArray (Tuple NonEmptyString fieldType)
recordToNEANEKV = rlToNEANEKV @rowList
