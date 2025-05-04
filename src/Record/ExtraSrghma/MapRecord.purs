module Record.ExtraSrghma.MapRecord where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder

mapRecord
  :: forall row rowList a b row'
   . RL.RowToList row rowList
  => MapRecord rowList row a b () row'
  => (a -> b)
  -> Record row
  -> Record row'
mapRecord f r = Builder.build builder {}
  where
  builder = mapRecordBuilder (Proxy :: Proxy rowList) f r

class
  MapRecord (rowList :: RL.RowList Type) (row :: Row Type) a b (from :: Row Type) (to :: Row Type)
  | rowList -> row a b from to where
  mapRecordBuilder :: Proxy rowList -> (a -> b) -> Record row -> Builder { | from } { | to }

instance mapRecordCons ::
  ( IsSymbol name
  , Row.Cons name a trash row
  , MapRecord tail row a b from from'
  , Row.Lacks name from'
  , Row.Cons name b from' to
  ) =>
  MapRecord (RL.Cons name a tail) row a b from to where
  mapRecordBuilder _ f r =
    first <<< rest
    where
    nameP = Proxy :: Proxy name
    val = f $ Record.get nameP r
    rest = mapRecordBuilder (Proxy :: Proxy tail) f r
    first = Builder.insert nameP val

instance mapRecordNil :: MapRecord RL.Nil row a b () () where
  mapRecordBuilder _ _ _ = identity
