module Record.ExtraSrghma.FoldrValuesWithIndex where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL

foldrValuesWithIndex
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValuesWithIndex rowList row fieldType
  => (String -> fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValuesWithIndex = foldrValuesWithIndexImpl (Proxy :: Proxy rowList)

foldMapValuesWithIndexR
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldrValuesWithIndex rowList row fieldType
  => Monoid accum
  => (String -> fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesWithIndexR f = foldrValuesWithIndex (\key x acc -> acc <> f key x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  )
  <= FoldrValuesWithIndex (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
    foldrValuesWithIndexImpl
      :: forall accum
       . Proxy rowList
      -> (String -> fieldType -> accum -> accum)
      -> accum
      -> Record row
      -> accum

instance foldrValuesWithIndexCons ::
  ( FoldrValuesWithIndex tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) => FoldrValuesWithIndex (RL.Cons name fieldType tailRowList) row fieldType
  where
    foldrValuesWithIndexImpl _ f accum record = f key value $ foldrValuesWithIndexImpl tailProxy f accum record
      where
        tailProxy :: Proxy tailRowList
        tailProxy = Proxy

        value :: fieldType
        value = Record.get (Proxy :: Proxy name) record

        key :: String
        key = reflectSymbol (Proxy :: Proxy name)

instance foldrValuesWithIndexNil
  :: Homogeneous row fieldType
  => FoldrValuesWithIndex RL.Nil row fieldType
  where
    foldrValuesWithIndexImpl _ _ accum _ = accum
