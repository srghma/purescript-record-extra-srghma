module Record.ExtraSrghma.Entries.List where

import Data.String.NonEmpty (NonEmptyString, class MakeNonEmpty, nes)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Data.Tuple (Tuple(..))
import Type.Prelude (class IsSymbol, reflectSymbol)

import Data.List (List(..))
import Data.List as List

-- LKV - List (Tuple String a)

class
  RLToLKV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToLKV :: Record row -> List (Tuple String fieldType)

instance RLToLKV RL.Nil row fieldType where
  rlToLKV _ = Nil

instance
  ( IsSymbol name
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToLKV tailRowList row fieldType
  ) =>
  RLToLKV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToLKV record =
    List.Cons (Tuple (reflectSymbol (Proxy :: Proxy name)) (Record.get (Proxy :: Proxy name) record))
      (rlToLKV @tailRowList record)

recordToLKV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToLKV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> List (Tuple String fieldType)
recordToLKV = rlToLKV @rowList

-- ANELKV - List (Tuple NonEmptyString a)

class
  RLToANELKV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToANELKV :: Record row -> List (Tuple NonEmptyString fieldType)

instance RLToANELKV RL.Nil row fieldType where
  rlToANELKV _ = Nil

instance
  ( IsSymbol name
  , MakeNonEmpty name
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToANELKV tailRowList row fieldType
  ) =>
  RLToANELKV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToANELKV record =
    List.Cons (Tuple (nes (Proxy :: Proxy name)) (Record.get (Proxy :: Proxy name) record))
      (rlToANELKV @tailRowList record)

recordToANELKV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToANELKV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> List (Tuple NonEmptyString fieldType)
recordToANELKV = rlToANELKV @rowList
