module Record.ExtraSrghma.Keys.Pick where

import Data.Function.Uncurried (Fn2, runFn2)
import Prim.Row as Row
import Prim.RowList as RL
import Record.ExtraSrghma.Keys.Array (class RowListToArrayOfKeys, rowListToArrayOfKeys)

foreign import pickFn :: forall r1 r2. Fn2 (Array String) (Record r1) (Record r2)

pick
  :: forall rowA r @rowB rowListB
   . Row.Union rowB r rowA
  => RL.RowToList rowB rowListB
  => RowListToArrayOfKeys rowListB
  => Record rowA
  -> Record rowB
pick = runFn2 pickFn (rowListToArrayOfKeys @rowListB)
