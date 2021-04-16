module Record.ExtraSrghma.FoldlValuesWithIndex where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL

foldlValuesWithIndex
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValuesWithIndex rowList row fieldType
  => (accum -> String -> fieldType -> accum)
  -> accum
  -> Record row
  -> accum
foldlValuesWithIndex = foldlValuesWithIndexImpl (Proxy :: Proxy rowList)

foldMapValuesWithIndexL
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValuesWithIndex rowList row fieldType
  => Monoid accum
  => (String -> fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesWithIndexL f = foldlValuesWithIndex (\acc key x -> acc <> f key x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  )
  <= FoldlValuesWithIndex (rowList :: RL.RowList Type) (row :: Row Type) fieldType
  | rowList -> row fieldType
  where
    foldlValuesWithIndexImpl
      :: forall accum
       . Proxy rowList
      -> (accum -> String -> fieldType -> accum)
      -> accum
      -> Record row
      -> accum

instance foldlValuesWithIndexCons ::
  ( FoldlValuesWithIndex tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) => FoldlValuesWithIndex (RL.Cons name fieldType tailRowList) row fieldType
  where
    foldlValuesWithIndexImpl _ f accum record = foldlValuesWithIndexImpl tailProxy f accum' record
      where
        tailProxy :: Proxy tailRowList
        tailProxy = Proxy

        value :: fieldType
        value = Record.get (Proxy :: Proxy name) record

        key :: String
        key = reflectSymbol (Proxy :: Proxy name)

        accum' = f accum key value

instance foldlValuesWithIndexNil
  :: Homogeneous row fieldType
  => FoldlValuesWithIndex RL.Nil row fieldType
  where
    foldlValuesWithIndexImpl _ _ accum _ = accum
