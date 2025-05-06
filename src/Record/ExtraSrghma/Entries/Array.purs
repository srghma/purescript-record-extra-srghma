module Record.ExtraSrghma.Entries.Array where

import Data.Array as Array
import Data.String.NonEmpty (NonEmptyString, class MakeNonEmpty, nes)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Data.Tuple (Tuple(..))
import Type.Prelude (class IsSymbol, reflectSymbol)

-- AKV - Array (Tuple String a)

class
  RLToAKV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToAKV :: Record row -> Array (Tuple String fieldType)

instance RLToAKV RL.Nil row fieldType where
  rlToAKV _ = []

instance
  ( IsSymbol name
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToAKV tailRowList row fieldType
  ) =>
  RLToAKV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToAKV record = Array.cons (Tuple name (Record.get (Proxy :: Proxy name) record)) (rlToAKV @tailRowList record)
    where
    name = reflectSymbol (Proxy :: Proxy name)

recordToAKV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToAKV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> Array (Tuple String fieldType)
recordToAKV = rlToAKV @rowList

-- ANEKV - Array (Tuple NonEmptyString a)

class
  RLToANEKV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToANEKV :: Record row -> Array (Tuple NonEmptyString fieldType)

instance RLToANEKV RL.Nil row fieldType where
  rlToANEKV _ = []

instance
  ( IsSymbol name
  , MakeNonEmpty name
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToANEKV tailRowList row fieldType
  ) =>
  RLToANEKV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToANEKV record =
    Array.cons (Tuple (nes (Proxy :: Proxy name)) (Record.get (Proxy :: Proxy name) record)) (rlToANEKV @tailRowList record)

recordToANEKV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToANEKV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> Array (Tuple NonEmptyString fieldType)
recordToANEKV = rlToANEKV @rowList
