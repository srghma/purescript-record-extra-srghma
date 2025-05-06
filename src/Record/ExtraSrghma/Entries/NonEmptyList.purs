module Record.ExtraSrghma.Entries.NonEmptyList where

import Data.String.NonEmpty (NonEmptyString, class MakeNonEmpty, nes)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.NonEmpty (NonEmpty, (:|))
import Type.Prelude (class IsSymbol, reflectSymbol)
import Record.ExtraSrghma.Entries.List (class RLToANELKV, class RLToLKV, rlToANELKV, rlToLKV)

-- NELKV - NonEmpty List (Tuple String a)

class
  RLToNELKV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToNELKV :: Record row -> NonEmpty List (Tuple String fieldType)

instance
  ( IsSymbol name
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToLKV tailRowList row fieldType
  ) =>
  RLToNELKV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToNELKV record =
    Tuple (reflectSymbol (Proxy :: Proxy name)) (Record.get (Proxy :: Proxy name) record)
      :| (rlToLKV @tailRowList record)

recordToNELKV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToNELKV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> NonEmpty List (Tuple String fieldType)
recordToNELKV = rlToNELKV @rowList

-- NEANELKV - NonEmpty List (Tuple NonEmptyString a)

class
  RLToNEANELKV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToNEANELKV :: Record row -> NonEmpty List (Tuple NonEmptyString fieldType)

instance
  ( IsSymbol name
  , MakeNonEmpty name
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToANELKV tailRowList row fieldType
  ) =>
  RLToNEANELKV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToNEANELKV record =
    Tuple (nes (Proxy :: Proxy name)) (Record.get (Proxy :: Proxy name) record)
      :| (rlToANELKV @tailRowList record)

recordToNEANELKV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToNEANELKV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> NonEmpty List (Tuple NonEmptyString fieldType)
recordToNEANELKV = rlToNEANELKV @rowList
