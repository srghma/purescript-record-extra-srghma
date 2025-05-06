module Record.ExtraSrghma.Values.NonEmptyArray where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.ExtraSrghma.Values.Array (class RLToAV, rlToAV)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

-- For converting RowList to array of values
class
  ( HomogeneousRowList rowList fieldType
  ) <=
  RLToNEAV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToNEAV :: Record row -> NonEmptyArray fieldType

instance
  ( IsSymbol name
  , HomogeneousRowList trash fieldType
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToAV tailRowList row fieldType
  ) =>
  RLToNEAV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToNEAV record = NonEmptyArray.cons' (Record.get (Proxy :: Proxy name) record) (rlToAV @tailRowList record)

-- Entry point: require row to be homogeneous
recordToNEAV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToNEAV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> NonEmptyArray fieldType
recordToNEAV = rlToNEAV @rowList
