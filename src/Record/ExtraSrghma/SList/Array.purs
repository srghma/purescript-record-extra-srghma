module Record.ExtraSrghma.SList.Array where

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Record.ExtraSrghma.SList.Types (SCons, SList, SNil)

-- 1. Array String
class SListToAS (slist :: SList) where
  slistToAS :: Array String

instance SListToAS SNil where
  slistToAS = []

instance (IsSymbol name, SListToAS tail) => SListToAS (SCons name tail) where
  slistToAS = Array.cons (reflectSymbol (Proxy :: Proxy name)) (slistToAS @tail)

-- 2. NonEmptyArray String
class SListToNEAS (slist :: SList) where
  slistToNEAS :: NonEmptyArray String

instance (IsSymbol name, SListToAS tail) => SListToNEAS (SCons name tail) where
  slistToNEAS = NonEmptyArray.cons' (reflectSymbol (Proxy :: Proxy name)) (slistToAS @tail)

-- 3. Array NonEmptyString
class SListToANES (slist :: SList) where
  slistToANES :: Array NonEmptyString

instance SListToANES SNil where
  slistToANES = []

instance (IsSymbol name, MakeNonEmpty name, SListToANES tail) => SListToANES (SCons name tail) where
  slistToANES = Array.cons (nes (Proxy :: Proxy name)) (slistToANES @tail)

-- 4. NonEmptyArray NonEmptyString
class SListToNEANES (slist :: SList) where
  slistToNEANES :: NonEmptyArray NonEmptyString

instance (IsSymbol name, MakeNonEmpty name, SListToANES tail) => SListToNEANES (SCons name tail) where
  slistToNEANES = NonEmptyArray.cons' (nes (Proxy :: Proxy name)) (slistToANES @tail)
