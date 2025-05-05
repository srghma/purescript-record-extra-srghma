module Record.ExtraSrghma.SList.NonEmptyArray where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Record.ExtraSrghma.SList.Array (class SListToANES, class SListToAS, slistToANES, slistToAS)
import Record.ExtraSrghma.SList.Types (SCons, SList)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- NonEmptyArray String (NEAS)
class SListToNEAS (slist :: SList) where
  slistToNEAS :: NonEmptyArray String

instance (IsSymbol name, SListToAS tail) => SListToNEAS (SCons name tail) where
  slistToNEAS = NonEmptyArray.cons' (reflectSymbol (Proxy :: Proxy name)) (slistToAS @tail)

-- NonEmptyArray NonEmptyString (NEANES)
class SListToNEANES (slist :: SList) where
  slistToNEANES :: NonEmptyArray NonEmptyString

instance (IsSymbol name, MakeNonEmpty name, SListToANES tail) => SListToNEANES (SCons name tail) where
  slistToNEANES = NonEmptyArray.cons' (nes (Proxy :: Proxy name)) (slistToANES @tail)
