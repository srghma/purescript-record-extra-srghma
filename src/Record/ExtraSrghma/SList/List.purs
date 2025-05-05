module Record.ExtraSrghma.SList.List where

import Prelude

import Data.List (List, (:))
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Record.ExtraSrghma.SList.Types (SCons, SList, SNil)

-- List String (LS)
class SListToLS (slist :: SList) where
  slistToLS :: List String

instance SListToLS SNil where
  slistToLS = mempty

instance (IsSymbol name, SListToLS tail) => SListToLS (SCons name tail) where
  slistToLS = reflectSymbol (Proxy :: Proxy name) : slistToLS @tail

-- List NonEmptyString (LNES)
class SListToLNES (slist :: SList) where
  slistToLNES :: List NonEmptyString

instance SListToLNES SNil where
  slistToLNES = mempty

instance (IsSymbol name, MakeNonEmpty name, SListToLNES tail) => SListToLNES (SCons name tail) where
  slistToLNES = nes (Proxy :: Proxy name) : slistToLNES @tail
