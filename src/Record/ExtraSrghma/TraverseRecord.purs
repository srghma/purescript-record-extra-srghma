module Record.ExtraSrghma.TraverseRecord where

import Prelude

import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, Proxy(..))

class TraverseRecord :: forall k. k -> Row Type -> Row Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class
  Functor m <=
  TraverseRecord rowList rowF rowA from to m
  | rowList -> rowF rowA from to m
  where
  traverseRecordImpl :: Record rowF -> Record rowA -> m (Builder { | from } { | to })

-- Empty case
instance Applicative m => TraverseRecord RL.Nil rowF rowA () () m where
  traverseRecordImpl _ _ = pure identity

-- Recursive case
else instance
  ( IsSymbol name
  , Row.Cons name (a -> m b) trashF rowF
  , Row.Cons name a trashA rowA
  , Apply m
  , TraverseRecord tail rowF rowA from from' m
  , Row.Lacks name from'
  , Row.Cons name b from' to
  ) =>
  TraverseRecord (RL.Cons name (a -> m b) tail) rowF rowA from to m where
  traverseRecordImpl fs as =
    fn <$> val <*> rest
    where
    namep = Proxy :: Proxy name
    f = Record.get namep fs
    a = Record.get namep as
    val = f a
    rest = traverseRecordImpl @tail fs as
    fn val' rest' = Builder.insert namep val' <<< rest'

-- User-facing function
traverseRecord
  :: forall rowF rowA rowOut rowList m
   . RL.RowToList rowF rowList
  => TraverseRecord rowList rowF rowA () rowOut m
  => Record rowF
  -> Record rowA
  -> m (Record rowOut)
traverseRecord fs as = Builder.build <@> {} <$> traverseRecordImpl @rowList fs as
