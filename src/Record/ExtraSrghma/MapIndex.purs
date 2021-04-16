module Record.ExtraSrghma.MapIndex where

import Prelude
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder

mapIndex :: forall row xs a b row'
   . RL.RowToList row xs
  => MapIndex xs row a b () row'
  => (String -> b)
  -> Proxy row
  -> Record row'
mapIndex f rowProxy = Builder.build builder {}
  where
    builder = mapIndexBuilder (Proxy :: Proxy xs) f

class MapIndex (xs :: RL.RowList Type) (row :: Row Type) a b (from :: Row Type) (to :: Row Type)
  | xs -> row a b from to where
  mapIndexBuilder :: Proxy xs -> (String -> b) -> Builder { | from } { | to }

instance mapIndexCons ::
  ( IsSymbol name
  , Row.Cons name a trash row
  , MapIndex tail row a b from from'
  , Row.Lacks name from'
  , Row.Cons name b from' to
  ) => MapIndex (RL.Cons name a tail) row a b from to where
  mapIndexBuilder _ f =
    first <<< rest
    where
      nameP = Proxy :: Proxy name
      val = f (reflectSymbol nameP)
      rest = mapIndexBuilder (Proxy :: Proxy tail) f
      first = Builder.insert nameP val

instance mapIndexNil :: MapIndex RL.Nil row a b () () where
  mapIndexBuilder _ _ = identity
