module Record.ExtraSrghma.Keys where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List, (:))
import Data.NonEmpty (NonEmpty, (:|))
import Prim.Row as Row
import Prim.RowList as RL
import Record.ExtraSrghma.SList (class SListToRowList)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

class RowListKeys (rowList :: RL.RowList Type) where
  rowListKeys :: List String

instance RowListKeys RL.Nil where
  rowListKeys = mempty

instance
  ( IsSymbol name
  , RowListKeys tail
  ) =>
  RowListKeys (RL.Cons name ty tail) where
  rowListKeys = first : rest
    where
    first = reflectSymbol (Proxy :: Proxy name)
    rest = rowListKeys @tail

class RowNonEmptyListKeys (rowList :: RL.RowList Type) where
  rowNonEmptyListKeys :: NonEmpty List String

instance
  ( IsSymbol name
  , RowListKeys tail
  ) =>
  RowNonEmptyListKeys (RL.Cons name ty tail) where
  rowNonEmptyListKeys = first :| rest
    where
    first = reflectSymbol (Proxy :: Proxy name)
    rest = rowListKeys @tail

rowKeys
  :: forall @row rowList
   . RL.RowToList row rowList
  => RowListKeys rowList
  => List String
rowKeys = rowListKeys @rowList

rowKeys1
  :: forall @row rowList
   . RL.RowToList row rowList
  => RowNonEmptyListKeys rowList
  => NonEmpty List String
rowKeys1 = rowNonEmptyListKeys @rowList

rowKeys'
  :: forall row rowList
   . RL.RowToList row rowList
  => RowListKeys rowList
  => Proxy row
  -> List String
rowKeys' _ = rowListKeys @rowList

rowKeys1'
  :: forall row rowList
   . RL.RowToList row rowList
  => RowNonEmptyListKeys rowList
  => Proxy row
  -> NonEmpty List String
rowKeys1' _ = rowNonEmptyListKeys @rowList

-- I had to use it. Also `recordKeys @{ bar :: String, baz :: Boolean }` throws error
class RecordKeys :: forall k. k -> Constraint
class RecordKeys a where
  recordKeys :: List String

instance (RL.RowToList row rowList, RowListKeys rowList) => RecordKeys (Record row) where
  recordKeys = rowKeys @row

-- same
class RecordKeys1 :: forall k. k -> Constraint
class RecordKeys1 a where
  recordKeys1 :: NonEmpty List String

instance (RL.RowToList row rowList, RowNonEmptyListKeys rowList) => RecordKeys1 (Record row) where
  recordKeys1 = rowKeys1 @row

-- Given a *proxy of a record*, return keys
recordKeys'
  :: forall row rowList
   . RL.RowToList row rowList
  => RowListKeys rowList
  => Proxy (Record row)
  -> List String
recordKeys' _ = rowListKeys @rowList

-- Given a *proxy of a record*, return keys
recordKeys1'
  :: forall row rowList
   . RL.RowToList row rowList
  => RowNonEmptyListKeys rowList
  => Proxy (Record row)
  -> NonEmpty List String
recordKeys1' _ = rowNonEmptyListKeys @rowList

foreign import pickFn :: forall r1 r2. Fn2 (Array String) (Record r1) (Record r2)

pick
  :: forall rowA r @rowB l
   . Row.Union rowB r rowA
  => RL.RowToList rowB l
  => RowListKeys l
  => Record rowA
  -> Record rowB
pick = runFn2 pickFn (Array.fromFoldable $ rowKeys @rowB)

slistKeys
  :: forall @slist rowList
   . SListToRowList slist rowList
  => RowListKeys rowList
  => List String
slistKeys = rowListKeys @rowList

slistKeys1
  :: forall @slist rowList
   . SListToRowList slist rowList
  => RowNonEmptyListKeys rowList
  => NonEmpty List String
slistKeys1 = rowNonEmptyListKeys @rowList

slistKeys'
  :: forall slist rowList
   . SListToRowList slist rowList
  => RowListKeys rowList
  => Proxy slist
  -> List String
slistKeys' _ = rowListKeys @rowList

slistKeys1'
  :: forall slist rowList
   . SListToRowList slist rowList
  => RowNonEmptyListKeys rowList
  => Proxy slist
  -> NonEmpty List String
slistKeys1' _ = rowNonEmptyListKeys @rowList
