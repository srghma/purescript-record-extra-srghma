module Record.ExtraSrghma.SequenceRecord where

import Prelude

import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, Proxy(..))

class SequenceRecord :: forall k. k -> Row Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class
  Functor m <=
  SequenceRecord rowList row from to m
  | rowList -> row from to m
  where
  sequenceRecordImpl :: Proxy rowList -> Record row -> m (Builder { | from } { | to })

-- Base case: single field in the row
instance sequenceRecordSingle ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Functor m
  , Row.Lacks name ()
  , Row.Cons name ty () to
  ) =>
  SequenceRecord (RL.Cons name (m ty) RL.Nil) row () to m where
  sequenceRecordImpl _ record =
    Builder.insert namep <$> valA
    where
    namep = Proxy :: Proxy name
    valA = Record.get namep record

-- Recursive case: process the tail of the record
else instance sequenceRecordCons ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Apply m
  , SequenceRecord tail row from from' m
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) =>
  SequenceRecord (RL.Cons name (m ty) tail) row from to m where
  sequenceRecordImpl _ record =
    fn <$> valA <*> rest
    where
    namep = Proxy :: Proxy name
    valA = Record.get namep record
    tailp = Proxy :: Proxy tail
    rest = sequenceRecordImpl tailp record
    fn valA' rest' = Builder.insert namep valA' <<< rest'

-- Base case: empty row, no fields to process
instance sequenceRecordNil :: Applicative m => SequenceRecord RL.Nil row () () m where
  sequenceRecordImpl _ _ = pure identity

-- User-facing function: build the sequence record
sequenceRecord
  :: forall row row' rowList m
   . RL.RowToList row rowList
  => SequenceRecord rowList row () row' m
  => Record row
  -> m (Record row')
sequenceRecord record = Builder.build <@> {} <$> builder
  where
  builder = sequenceRecordImpl (Proxy :: Proxy rowList) record
