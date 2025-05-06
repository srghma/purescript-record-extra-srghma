module Record.ExtraSrghma.Values.NonEmptyList where

import Data.NonEmpty (NonEmpty, (:|))

import Data.List (List)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.ExtraSrghma.Values.List (class RLToLV, rlToLV)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

-- For converting RowList to array of values
class
  ( HomogeneousRowList rowList fieldType
  ) <=
  RLToNELV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToNELV :: Record row -> NonEmpty List fieldType

instance
  ( IsSymbol name
  , HomogeneousRowList trash fieldType
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToLV tailRowList row fieldType
  ) =>
  RLToNELV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToNELV record = (Record.get (Proxy :: Proxy name) record) :| (rlToLV @tailRowList record)

-- Entry point: require row to be homogeneous
recordToNELV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToNELV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> NonEmpty List fieldType
recordToNELV = rlToNELV @rowList
