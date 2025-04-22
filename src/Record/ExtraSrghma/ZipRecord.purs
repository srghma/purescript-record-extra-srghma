module Record.ExtraSrghma.ZipRecord where

import Prelude

import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, Proxy(..))

class
  ZipRecord
    (rla :: RL.RowList Type)
    (ra :: Row Type)
    (rlb :: RL.RowList Type)
    (rb :: Row Type)
    (from :: Row Type)
    (to :: Row Type)
  | rla -> ra from to
  , rlb -> rb from to
  where
  zipRecordImpl
    :: Proxy rla
    -> Record ra
    -> Proxy rlb
    -> Record rb
    -> Builder { | from } { | to }

instance zipRecordNil :: ZipRecord RL.Nil trashA RL.Nil trashB () ()
  where
  zipRecordImpl _ _ _ _ = identity

instance zipRecordCons ::
  ( IsSymbol k
  , Row.Cons k a trashA ra
  , Row.Cons k b trashB rb
  , Row.Cons k (Tuple a b) from' to
  , Row.Lacks k from'
  , ZipRecord ta ra tb rb from from'
  ) =>
  ZipRecord
    (RL.Cons k a ta)
    ra
    (RL.Cons k b tb)
    rb
    from
    to
  where
  zipRecordImpl _ ra _ rb = first <<< tail
    where
    name = Proxy :: Proxy k
    head = Tuple (Record.get name ra) (Record.get name rb)
    ta = Proxy :: Proxy ta
    tb = Proxy :: Proxy tb
    tail = zipRecordImpl ta ra tb rb
    first = Builder.insert name head

zipRecord
  :: forall ta ra tb rb rc
   . RL.RowToList ra ta
  => RL.RowToList rb tb
  => ZipRecord ta ra tb rb () rc
  => Record ra
  -> Record rb
  -> Record rc
zipRecord ra rb = Builder.build builder {}
  where
  ta = Proxy :: Proxy ta
  tb = Proxy :: Proxy tb
  builder = zipRecordImpl ta ra tb rb
