module Record.ExtraSrghma.Keys where

import Prelude
import Record.ExtraSrghma.CompareRecord (class SListToRowList)
import Data.List (List, (:))
import Data.Array (fromFoldable)
import Data.Function.Uncurried (Fn2, runFn2)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

class Keys (rowList :: RL.RowList Type) where
  keysImpl :: List String

instance nilKeys :: Keys RL.Nil where
  keysImpl = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) =>
  Keys (RL.Cons name ty tail) where
  keysImpl = first : rest
    where
    first = reflectSymbol (Proxy :: Proxy name)
    rest = keysImpl @tail

keys
  :: forall @row rl
   . RL.RowToList row rl
  => Keys rl
  => List String
keys = keysImpl @rl

foreign import pickFn :: forall r1 r2. Fn2 (Array String) (Record r1) (Record r2)

pick
  :: forall a r b l
   . Row.Union b r a
  => RL.RowToList b l
  => Keys l
  => Record a
  -> Record b
pick = runFn2 pickFn ks
  where
  ks = fromFoldable $ keys @b

slistKeys
  :: forall @tuples rl
   . SListToRowList tuples rl
  => Keys rl
  => List String
slistKeys = keysImpl @rl
