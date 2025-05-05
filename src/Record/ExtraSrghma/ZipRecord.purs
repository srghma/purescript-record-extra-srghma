module Record.ExtraSrghma.ZipRecord where

import Prelude

import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, Proxy(..))

-- | Zips two records (with matching keys) into a single record of Tuple values.
class
  ZipRecord
    (rowListL :: RL.RowList Type)
    (rowListR :: RL.RowList Type)
    (rowL :: Row Type)
    (rowR :: Row Type)
    (resultRowAcc :: Row Type)
    (resultRow :: Row Type)
  | rowListL -> rowL resultRowAcc resultRow
  , rowListR -> rowR resultRowAcc resultRow
  where
  zipRecordImpl
    :: Record rowL
    -> Record rowR
    -> Builder { | resultRowAcc } { | resultRow }

-- Base case: both records are empty
instance ZipRecord RL.Nil RL.Nil rowL rowR () () where
  zipRecordImpl _ _ = identity

-- Recursive case: zip head keys into Tuple, and recurse
instance
  ( IsSymbol key
  , Row.Cons key a restRowL rowL
  , Row.Cons key b restRowR rowR
  , Row.Cons key (Tuple a b) resultTail resultRow
  , Row.Lacks key resultTail
  , ZipRecord tailL tailR rowL rowR resultRowAcc resultTail
  ) =>
  ZipRecord
    (RL.Cons key a tailL)
    (RL.Cons key b tailR)
    rowL
    rowR
    resultRowAcc
    resultRow
  where
  zipRecordImpl recordL recordR =
    Builder.insert
      (Proxy :: Proxy key)
      (Tuple (Record.get (Proxy :: Proxy key) recordL) (Record.get (Proxy :: Proxy key) recordR))
      <<< zipRecordImpl @tailL @tailR recordL recordR

-- User-facing function: builds the zipped record
zipRecord
  :: forall rowListL rowL rowListR rowR zippedRow
   . RL.RowToList rowL rowListL
  => RL.RowToList rowR rowListR
  => ZipRecord rowListL rowListR rowL rowR () zippedRow
  => Record rowL
  -> Record rowR
  -> Record zippedRow
zipRecord recordL recordR = Builder.buildFromScratch (zipRecordImpl @rowListL @rowListR recordL recordR)
