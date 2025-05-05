module Record.ExtraSrghma.SList.List where

import Prelude

import Data.List (List, (:))
import Data.NonEmpty (NonEmpty, (:|))
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Record.ExtraSrghma.SList.Types (SCons, SList, SNil)

-- 1. List String
class SListToLS (slist :: SList) where
  slistToLS :: List String

instance SListToLS SNil where
  slistToLS = mempty

instance (IsSymbol name, SListToLS tail) => SListToLS (SCons name tail) where
  slistToLS = reflectSymbol (Proxy :: Proxy name) : slistToLS @tail

-- 2. NonEmpty List String
class SListToNELS (slist :: SList) where
  slistToNELS :: NonEmpty List String

instance (IsSymbol name, SListToLS tail) => SListToNELS (SCons name tail) where
  slistToNELS = reflectSymbol (Proxy :: Proxy name) :| slistToLS @tail

-- 3. List NonEmptyString
class SListToLNES (slist :: SList) where
  slistToLNES :: List NonEmptyString

instance SListToLNES SNil where
  slistToLNES = mempty

instance (IsSymbol name, MakeNonEmpty name, SListToLNES tail) => SListToLNES (SCons name tail) where
  slistToLNES = nes (Proxy :: Proxy name) : slistToLNES @tail

-- 4. NonEmpty List NonEmptyString
class SListToNELNES (slist :: SList) where
  slistToNELNES :: NonEmpty List NonEmptyString

instance (IsSymbol name, MakeNonEmpty name, SListToLNES tail) => SListToNELNES (SCons name tail) where
  slistToNELNES = nes (Proxy :: Proxy name) :| slistToLNES @tail
