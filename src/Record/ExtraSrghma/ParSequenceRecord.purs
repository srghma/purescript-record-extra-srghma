module Record.ExtraSrghma.ParSequenceRecord where

import Prelude

import Record as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Control.Parallel (class Parallel, parallel, sequential)

-- | Sequences a record of monadic values in parallel, returning a monadic record of values.
-- | For example, turns `{ a: Aff Int, b: Aff String }` into `Aff { a: Int, b: String }`
-- | by running all the monadic values in parallel.
parSequenceRecord
  :: forall inputRow outputRow rowList parM m
   . RL.RowToList inputRow rowList
  => ParSequenceRecord rowList inputRow () outputRow parM m
  => Record inputRow
  -> m (Record outputRow)
parSequenceRecord record = sequential $ Builder.build <@> {} <$> builderAction
  where
  builderAction = parSequenceRecordImpl @rowList record

-- | Class for sequencing a record of monadic values in parallel
class ParSequenceRecord :: forall k. k -> Row Type -> Row Type -> Row Type -> (Type -> Type) -> (Type -> Type) -> Constraint
class
  Parallel parM m <=
  ParSequenceRecord rowList inputRow accRow resultRow parM m
  | rowList -> inputRow accRow resultRow parM m
  where
  parSequenceRecordImpl :: Record inputRow -> parM (Builder { | accRow } { | resultRow })

-- | Base case: When there's only one field left in the record
instance singleFieldSequencer ::
  ( IsSymbol fieldName
  , Row.Cons fieldName (m fieldType) remainingFields inputRow
  , Parallel parM m
  , Row.Lacks fieldName ()
  , Row.Cons fieldName fieldType () resultRow
  ) =>
  ParSequenceRecord (RL.Cons fieldName (m fieldType) RL.Nil) inputRow () resultRow parM m where
  parSequenceRecordImpl record = Builder.insert fieldProxy <$> parallelFieldValue
    where
    fieldProxy = Proxy :: Proxy fieldName

    parallelFieldValue :: parM fieldType
    parallelFieldValue = parallel $ Record.get fieldProxy record

-- | Recursive case: Handle multiple fields in the record
else instance multipleFieldsSequencer ::
  ( IsSymbol fieldName
  , Row.Cons fieldName (m fieldType) remainingFields inputRow
  , ParSequenceRecord tailList inputRow accRow intermediateRow parM m
  , Row.Lacks fieldName intermediateRow
  , Row.Cons fieldName fieldType intermediateRow resultRow
  ) =>
  ParSequenceRecord (RL.Cons fieldName (m fieldType) tailList) inputRow accRow resultRow parM m where
  parSequenceRecordImpl record = combineResults <$> parallelFieldValue <*> restOfFields
    where
    fieldProxy = Proxy :: Proxy fieldName

    parallelFieldValue :: parM fieldType
    parallelFieldValue = parallel $ Record.get fieldProxy record

    restOfFields :: parM (Builder (Record accRow) (Record intermediateRow))
    restOfFields = parSequenceRecordImpl @tailList record

    combineResults :: fieldType -> Builder (Record accRow) (Record intermediateRow) -> Builder (Record accRow) (Record resultRow)
    combineResults fieldValue restBuilder = Builder.insert fieldProxy fieldValue <<< restBuilder

-- | Base case: Empty record
instance emptyRecordSequencer ::
  ( Parallel parM m
  , Applicative parM
  ) =>
  ParSequenceRecord RL.Nil inputRow () () parM m where
  parSequenceRecordImpl _ = pure identity
