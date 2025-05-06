module Record.ExtraSrghma.Values.List where

import Data.List (List(..))
import Data.List as List
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)

-- RowList to List
class
  ( HomogeneousRowList rowList fieldType
  ) <=
  RLToLV (rowList :: RL.RowList Type) (row :: Row Type) (fieldType :: Type)
  | rowList -> row fieldType where
  rlToLV :: Record row -> List fieldType

instance Homogeneous row fieldType => RLToLV RL.Nil row fieldType where
  rlToLV _ = Nil

instance
  ( IsSymbol name
  , HomogeneousRowList trash fieldType
  , Row.Cons name fieldType tailRow row
  , RL.RowToList row trash
  , RLToLV tailRowList row fieldType
  ) =>
  RLToLV (RL.Cons name fieldType tailRowList) row fieldType where
  rlToLV record = List.Cons (Record.get (Proxy :: Proxy name) record) (rlToLV @tailRowList record)

-- Entry point
recordToLV
  :: forall @row rowList fieldType
   . RL.RowToList row rowList
  => RLToLV rowList row fieldType
  => Homogeneous row fieldType
  => Record row
  -> List fieldType
recordToLV = rlToLV @rowList
