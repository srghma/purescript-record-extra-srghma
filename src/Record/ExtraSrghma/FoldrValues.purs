module Record.ExtraSrghma.FoldrValues where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL

foldrValues
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValues rowList row fieldType
  => (fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValues = foldrValuesImpl (Proxy :: Proxy rowList)

foldMapValuesR
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValues rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesR f = foldrValues (\x acc -> acc <> f x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  )
  <= FoldrValues (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
    foldrValuesImpl
      :: forall accum
       . Proxy rowList
      -> (fieldType -> accum -> accum)
      -> accum
      -> Record row
      -> accum

instance foldrValuesCons ::
  ( FoldrValues tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) => FoldrValues (RL.Cons name fieldType tailRowList) row fieldType
  where
    foldrValuesImpl _ f accum record = f value $ foldrValuesImpl tailProxy f accum record
      where
        tailProxy :: Proxy tailRowList
        tailProxy = Proxy

        value :: fieldType
        value = Record.get (Proxy :: Proxy name) record

instance foldrValuesNil
  :: Homogeneous row fieldType
  => FoldrValues RL.Nil row fieldType
  where
    foldrValuesImpl _ _ accum _ = accum

