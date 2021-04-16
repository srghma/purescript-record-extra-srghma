module Record.ExtraSrghma.MapValuesWithIndex where

import Prelude

import Record as Record
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder

-- Like `mapRecord` from https://github.com/justinwoo/purescript-record-extra
-- but `mapRecordWithIndex`

mapValuesWithIndex :: forall row xs a b row'
   . RL.RowToList row xs
  => MapValuesWithIndex xs row a b () row'
  => (String -> a -> b)
  -> Record row
  -> Record row'
mapValuesWithIndex f r = Builder.build builder {}
  where
    builder = mapValuesWithIndexBuilder (Proxy :: Proxy xs) f r

class MapValuesWithIndex (xs :: RL.RowList Type) (row :: Row Type) a b (from :: Row Type) (to :: Row Type)
  | xs -> row a b from to where
  mapValuesWithIndexBuilder :: Proxy xs -> (String -> a -> b) -> Record row -> Builder { | from } { | to }

instance mapValuesWithIndexCons ::
  ( IsSymbol name
  , Row.Cons name a trash row
  , MapValuesWithIndex tail row a b from from'
  , Row.Lacks name from'
  , Row.Cons name b from' to
  ) => MapValuesWithIndex (RL.Cons name a tail) row a b from to where
  mapValuesWithIndexBuilder _ f r =
    first <<< rest
    where
      nameP = Proxy :: Proxy name
      val = f (reflectSymbol nameP) (Record.get nameP r)
      rest = mapValuesWithIndexBuilder (Proxy :: Proxy tail) f r
      first = Builder.insert nameP val

instance mapValuesWithIndexNil :: MapValuesWithIndex RL.Nil row a b () () where
  mapValuesWithIndexBuilder _ _ _ = identity
