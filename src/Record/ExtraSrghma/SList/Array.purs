module Record.ExtraSrghma.SList.Array where

import Data.Array as Array
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Record.ExtraSrghma.SList.Types (SCons, SList, SNil)

-- Array String (AS)
class SListToAS (slist :: SList) where
  slistToAS :: Array String

instance SListToAS SNil where
  slistToAS = []

instance (IsSymbol name, SListToAS tail) => SListToAS (SCons name tail) where
  slistToAS = Array.cons (reflectSymbol (Proxy :: Proxy name)) (slistToAS @tail)

-- Array NonEmptyString (ANES)
class SListToANES (slist :: SList) where
  slistToANES :: Array NonEmptyString

instance SListToANES SNil where
  slistToANES = []

instance (IsSymbol name, MakeNonEmpty name, SListToANES tail) => SListToANES (SCons name tail) where
  slistToANES = Array.cons (nes (Proxy :: Proxy name)) (slistToANES @tail)
