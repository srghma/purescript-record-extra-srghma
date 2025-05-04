module Record.ExtraSrghma.MapIndex where

import Prelude
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder

mapIndex
  :: forall row rowList a b row'
   . RL.RowToList row rowList
  => MapIndex rowList row a b () row'
  => (String -> b)
  -> Proxy row
  -> Record row'
mapIndex f _ = Builder.build builder {}
  where
  builder = mapIndexBuilder @rowList f

class MapIndex :: forall k. RL.RowList Type -> Row Type -> k -> Type -> Row Type -> Row Type -> Constraint
class
  MapIndex (rowList :: RL.RowList Type) (row :: Row Type) a b (from :: Row Type) (to :: Row Type)
  | rowList -> row a b from to where
  mapIndexBuilder :: (String -> b) -> Builder { | from } { | to }

instance
  ( IsSymbol name
  , Row.Cons name a trash row
  , MapIndex tail row a b from from'
  , Row.Lacks name from'
  , Row.Cons name b from' to
  ) =>
  MapIndex (RL.Cons name a tail) row a b from to where
  mapIndexBuilder f =
    first <<< rest
    where
    nameP = Proxy :: Proxy name
    val = f (reflectSymbol nameP)
    rest = mapIndexBuilder @tail f
    first = Builder.insert nameP val

instance MapIndex RL.Nil row a b () () where
  mapIndexBuilder _ = identity
