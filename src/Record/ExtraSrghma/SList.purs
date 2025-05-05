module Record.ExtraSrghma.SList where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.List (List, (:))
import Data.NonEmpty (NonEmpty, (:|))
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

-- | A phantom type-level linked list of field names (symbols), used to express a deterministic ordering.
data SList

foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList

infixr 6 type SCons as :::

--------------------------
-- | Convert a type-level symbol list (`SList`) to a RowList (`RL.RowList`).
-- | Example use `foo :: forall @slist rowList . SListToRowList slist rowList => ...do something with rowList`
class SListToRowList (xs :: SList) (rl :: RL.RowList Type) | xs -> rl, rl -> xs

instance SListToRowList SNil RL.Nil
instance SListToRowList sTail tail => SListToRowList (SCons name sTail) (RL.Cons name trash tail)

--------------------------
-- List S
class SListToListOfStrings (slist :: SList) where
  slistToListOfStrings :: List String

instance SListToListOfStrings SNil where
  slistToListOfStrings = mempty

instance (IsSymbol name, SListToListOfStrings tail) => SListToListOfStrings (SCons name tail) where
  slistToListOfStrings = reflectSymbol (Proxy :: Proxy name) : slistToListOfStrings @tail

class SListToListOfStrings1 (slist :: SList) where
  slistToListOfStrings1 :: NonEmpty List String

instance (IsSymbol name, SListToListOfStrings tail) => SListToListOfStrings1 (SCons name tail) where
  slistToListOfStrings1 = reflectSymbol (Proxy :: Proxy name) :| slistToListOfStrings @tail

-- Array S

class SListToArrayOfStrings (slist :: SList) where
  slistToArrayOfStrings :: Array String

instance SListToArrayOfStrings SNil where
  slistToArrayOfStrings = []

instance (IsSymbol name, SListToArrayOfStrings tail) => SListToArrayOfStrings (SCons name tail) where
  slistToArrayOfStrings = Array.cons (reflectSymbol (Proxy :: Proxy name)) (slistToArrayOfStrings @tail)

class SListToArrayOfStrings1 (slist :: SList) where
  slistToArrayOfStrings1 :: NonEmptyArray String

instance (IsSymbol name, SListToArrayOfStrings tail) => SListToArrayOfStrings1 (SCons name tail) where
  slistToArrayOfStrings1 = NonEmptyArray.cons' (reflectSymbol (Proxy :: Proxy name)) (slistToArrayOfStrings @tail)

--------------------------
-- List NES
class SListToListOfNonEmptyStrings (slist :: SList) where
  slistToListOfNonEmptyStrings :: List NonEmptyString

instance SListToListOfNonEmptyStrings SNil where
  slistToListOfNonEmptyStrings = mempty

instance (IsSymbol name, MakeNonEmpty name, SListToListOfNonEmptyStrings tail) => SListToListOfNonEmptyStrings (SCons name tail) where
  slistToListOfNonEmptyStrings = nes (Proxy :: Proxy name) : slistToListOfNonEmptyStrings @tail

class SListToListOfNonEmptyStrings1 (slist :: SList) where
  slistToListOfNonEmptyStrings1 :: NonEmpty List NonEmptyString

instance (IsSymbol name, MakeNonEmpty name, SListToListOfNonEmptyStrings tail) => SListToListOfNonEmptyStrings1 (SCons name tail) where
  slistToListOfNonEmptyStrings1 = nes (Proxy :: Proxy name) :| slistToListOfNonEmptyStrings @tail

-- Array NES

class SListToArrayOfNonEmptyStrings (slist :: SList) where
  slistToArrayOfNonEmptyStrings :: Array NonEmptyString

instance SListToArrayOfNonEmptyStrings SNil where
  slistToArrayOfNonEmptyStrings = []

instance (IsSymbol name, MakeNonEmpty name, SListToArrayOfNonEmptyStrings tail) => SListToArrayOfNonEmptyStrings (SCons name tail) where
  slistToArrayOfNonEmptyStrings = Array.cons (nes (Proxy :: Proxy name)) (slistToArrayOfNonEmptyStrings @tail)

class SListToArrayOfNonEmptyStrings1 (slist :: SList) where
  slistToArrayOfNonEmptyStrings1 :: NonEmptyArray NonEmptyString

instance (IsSymbol name, MakeNonEmpty name, SListToArrayOfNonEmptyStrings tail) => SListToArrayOfNonEmptyStrings1 (SCons name tail) where
  slistToArrayOfNonEmptyStrings1 = NonEmptyArray.cons' (nes (Proxy :: Proxy name)) (slistToArrayOfNonEmptyStrings @tail)
