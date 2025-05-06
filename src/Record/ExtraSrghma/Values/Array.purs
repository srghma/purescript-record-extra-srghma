module Record.ExtraSrghma.Values.Array where

import Data.Array as Array
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

-- For converting RowList to array of values
class
  ( HomogeneousRowList rowList fieldType
  ) <=
  RLToAV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToAV :: Record row -> Array fieldType

instance Homogeneous row fieldType => RLToAV RL.Nil row fieldType where
  rlToAV _ = []

instance
  ( IsSymbol name
  , HomogeneousRowList trash fieldType
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToAV tailRowList row fieldType
  ) =>
  RLToAV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToAV record = Array.cons (Record.get (Proxy :: Proxy name) record) (rlToAV @tailRowList record)

-- Entry point: require row to be homogeneous
recordToAV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToAV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> Array fieldType
recordToAV = rlToAV @rowList
