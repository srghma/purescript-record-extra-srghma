module Record.ExtraSrghma.AppendRecord where

import Prelude

import Record as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Prim.Row as Row
import Prim.RowList as RL

-- How it differs from [SemigroupRecord](https://pursuit.purescript.org/packages/purescript-prelude/6.0.2/docs/Data.Semigroup#t:SemigroupRecord)?
-- Records dont need to have equal number of elements!
-- Check tests for more info.
class AppendSubrecordImpl :: forall k. k -> Row Type -> Row Type -> Constraint
class AppendSubrecordImpl rl bigger smaller where
  appendSubrecordImpl :: Record bigger -> Record smaller -> Record bigger

instance AppendSubrecordImpl RL.Nil bigger smaller where
  appendSubrecordImpl b _ = b

instance
  ( IsSymbol name
  , Row.Cons name t trash smaller
  , Row.Cons name t trash' bigger
  , Semigroup t
  , AppendSubrecordImpl tail bigger smaller
  ) =>
  AppendSubrecordImpl (RL.Cons name t tail) bigger smaller where
  appendSubrecordImpl bigger smaller = Record.modify key modifier rest
    where
    key = Proxy :: Proxy name
    modifier v = v <> Record.get key smaller
    rest = appendSubrecordImpl @tail bigger smaller

appendRecord
  :: forall rl bigger smaller
   . RL.RowToList smaller rl
  => AppendSubrecordImpl rl bigger smaller
  => Record bigger
  -> Record smaller
  -> Record bigger
appendRecord b s = appendSubrecordImpl @rl b s
