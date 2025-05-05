module Record.ExtraSrghma.SList.NonEmptyList where

import Data.List (List)
import Data.NonEmpty (NonEmpty, (:|))
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Record.ExtraSrghma.SList.Types (SCons, SList)
import Record.ExtraSrghma.SList.List (class SListToLNES, class SListToLS, slistToLNES, slistToLS)

-- NonEmpty List String (NELS)
class SListToNELS (slist :: SList) where
  slistToNELS :: NonEmpty List String

instance (IsSymbol name, SListToLS tail) => SListToNELS (SCons name tail) where
  slistToNELS = reflectSymbol (Proxy :: Proxy name) :| slistToLS @tail

-- NonEmpty List NonEmptyString (NELNES)
class SListToNELNES (slist :: SList) where
  slistToNELNES :: NonEmpty List NonEmptyString

instance (IsSymbol name, MakeNonEmpty name, SListToLNES tail) => SListToNELNES (SCons name tail) where
  slistToNELNES = nes (Proxy :: Proxy name) :| slistToLNES @tail
