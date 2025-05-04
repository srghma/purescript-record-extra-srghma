module Record.ExtraSrghma.CompareRecord where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Prim.Row as Row
import Prim.RowList as RL

data SList

foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList

infixr 6 type SCons as :::

class SListToRowList (xs :: SList) (rl :: RL.RowList Type) | xs -> rl, rl -> xs

instance slToRlSNil :: SListToRowList SNil RL.Nil

instance slToRlSCons ::
  ( SListToRowList sTail tail
  ) =>
  SListToRowList (SCons name sTail) (RL.Cons name trash tail)

class OrdRecord :: forall k. k -> Row Type -> Constraint
class
  OrdRecord rl row
  | rl -> row
  where
  compareRecordImpl :: Record row -> Record row -> Ordering

instance ordRecordCons ::
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

instance ordRecordNil :: OrdRecord RL.Nil row where
  compareRecordImpl _ _ = EQ

compareRecord
  :: forall row rl
   . RL.RowToList row rl
  => OrdRecord rl row
  => Record row
  -> Record row
  -> Ordering
compareRecord a b = compareRecordImpl @rl a b
