module Record.ExtraSrghma.ValuesToUnfoldableLazy where

import Prelude

import Control.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Prim.RowList as RL
import Record.ExtraSrghma.FoldrValuesLazy (class FoldrValuesLazy, foldrValuesLazy)

-- | A lazy unfoldable wrapper for converting record values into a sequential structure.
newtype LazyTupleList a =
  LazyTupleList (Unit -> Maybe (Tuple a (LazyTupleList a)))

derive newtype instance Lazy.Lazy (LazyTupleList a)

-- | Converts a record of values into any `Unfoldable` container, lazily.
--
-- This avoids evaluating all fields immediately — only what's needed to build the result.
valuesToUnfoldableLazy
  :: forall row rowList container value
   . RL.RowToList row rowList
  => FoldrValuesLazy rowList row value
  => Unfoldable container
  => Record row
  -> container value
valuesToUnfoldableLazy record =
  unfoldr (\(LazyTupleList next) -> next unit) stream
  where
  stream = foldrValuesLazy
    (\value acc -> LazyTupleList (\_ -> Just (Tuple value acc)))
    (LazyTupleList (\_ -> Nothing))
    record

-- -- | Converts a record of values into any `Unfoldable1` container, lazily.
-- --
-- -- This avoids evaluating all fields immediately — only what's needed to build the result.
-- valuesToUnfoldable1Lazy
--   :: forall row rowList container value
--    . RL.RowToList row rowList
--   => FoldrValuesLazy rowList row value
--   => Unfoldable1 container
--   => Record row
--   -> container value
-- valuesToUnfoldable1Lazy record =
--   unfoldr1 (\(LazyTupleList next) -> Tuple <$> next unit <*> pure Nothing) stream
--   where
--   stream = foldrValuesLazy1
--     (\value acc -> LazyTupleList (\_ -> Just (Tuple value acc)))
--     (LazyTupleList (\_ -> Nothing))
--     record
