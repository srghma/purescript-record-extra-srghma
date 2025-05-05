module Record.ExtraSrghma.SList.Types where

import Prim.RowList as RL

-- | A phantom type-level linked list of field names (symbols), used to express a deterministic ordering.
data SList

foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList

infixr 6 type SCons as :::

--------------------------
-- | Convert a type-level symbol list (`SList`) to a RowList (`RL.RowList`).
-- | Example use `foo :: forall @slist rowList . SListToRowList slist rowList => ...do something with rowList`
class SListToRowList (xs :: SList) (rl :: RL.RowList Type) | xs -> rl, rl -> xs

instance SListToRowList SNil RL.Nil
instance SListToRowList sTail tail => SListToRowList (SCons name sTail) (RL.Cons name trash tail)
