module Record.ExtraSrghma.Keys where

import Prelude
import Record.ExtraSrghma.CompareRecord
import Data.List (List, (:))
import Data.Array (fromFoldable)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

class Keys (xs :: RL.RowList Type) where
  keysImpl :: Proxy xs -> List String

instance nilKeys :: Keys RL.Nil where
  keysImpl _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (RL.Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol (Proxy :: Proxy name)
      rest = keysImpl (Proxy :: Proxy tail)

keys :: forall g row rl
   . RL.RowToList row rl
  => Keys rl
  => g row -- this will work for any type with the row as a param!
  -> List String
keys _ = keysImpl (Proxy :: Proxy rl)

foreign import pickFn :: forall r1 r2. Fn2 (Array String) (Record r1) (Record r2)

pick :: forall a r b l.
     Row.Union b r a
  => RL.RowToList b l
  => Keys l
  => Record a
  -> Record b
pick = runFn2 pickFn ks
  where
    ks = fromFoldable $ keys (Proxy :: Proxy b)

slistKeys :: forall g tuples rl
   . SListToRowList tuples rl
  => Keys rl
  => g tuples
  -> List String
slistKeys _ = keysImpl (Proxy :: Proxy rl)
