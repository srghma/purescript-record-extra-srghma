module Record.ExtraSrghma.Pop where

import Type.Prelude (class IsSymbol, SProxy)import Type.Prelude (class IsSymbol, SProxy)
import Prim.Row as Row
import Record as Record
import Data.Tuple (Tuple(..))

pop :: forall label a withoutRow inputRow
   . IsSymbol label
  => Row.Cons label a withoutRow inputRow
  => Row.Lacks label withoutRow
  => SProxy label
  -> Record inputRow
  -> (Tuple a (Record withoutRow))
pop label record = Tuple (Record.get label record) (Record.delete label record)
