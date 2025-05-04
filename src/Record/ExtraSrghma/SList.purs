module Record.ExtraSrghma.SList where

import Prim.RowList as RL

-- | A phantom type-level linked list of field names (symbols), used to express a deterministic ordering.
data SList

-- | A field name followed by the rest of the list.
foreign import data SCons :: Symbol -> SList -> SList

-- | Empty field name list.
foreign import data SNil :: SList

-- | Infix alias for SCons.
infixr 6 type SCons as :::

-- | Convert a type-level symbol list (`SList`) to a RowList (`RL.RowList`).
class SListToRowList (xs :: SList) (rl :: RL.RowList Type) | xs -> rl, rl -> xs

instance SListToRowList SNil RL.Nil

instance
  ( SListToRowList sTail tail
  ) =>
  SListToRowList (SCons name sTail) (RL.Cons name trash tail)
