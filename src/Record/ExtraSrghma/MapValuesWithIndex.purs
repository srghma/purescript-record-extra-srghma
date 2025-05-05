module Record.ExtraSrghma.MapValuesWithIndex where

import Prelude

import Record as Record
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder

-- | Like `mapRecord` from https://github.com/justinwoo/purescript-record-extra
-- | but `mapRecordWithIndex`
-- | The 'mapValuesWithIndex' function allows you to transform the values of a record,
-- | with access to the index (as the field name) and the value.
-- |
-- | Example usage:
-- |
-- | ```purescript
-- | let record = { name: "Alice", age: 25 }
-- | let result = mapValuesWithIndex (\name value -> name ++ ": " ++ show value) record
-- | -- result will be: { name: "name: Alice", age: "age: 25" }
-- | ```
mapValuesWithIndex
  :: forall row rowList a b row'
   . RL.RowToList row rowList
  => MapValuesWithIndex rowList row a b () row'
  => (String -> a -> b)
  -> Record row
  -> Record row'
mapValuesWithIndex transform record = Builder.buildFromScratch builder
  where
  builder = mapValuesWithIndexBuilder @rowList transform record

-- | Type class for transforming record values based on field names and values.
class
  MapValuesWithIndex (rowList :: RL.RowList Type) (row :: Row Type) a b (from :: Row Type) (to :: Row Type)
  | rowList -> row a b from to where
  -- | The implementation for mapping over the record's values with the field name.
  mapValuesWithIndexBuilder :: (String -> a -> b) -> Record row -> Builder { | from } { | to }

-- | Instance for transforming a record with a non-empty field (i.e., head of the row list).
instance
  ( IsSymbol fieldName
  , Row.Cons fieldName a restRow row
  , MapValuesWithIndex tail row a b from from'
  , Row.Lacks fieldName from'
  , Row.Cons fieldName b from' to
  ) =>
  MapValuesWithIndex (RL.Cons fieldName a tail) row a b from to where
  mapValuesWithIndexBuilder transform record =
    applyTransformation <<< restTransformation
    where
    fieldProxy = Proxy :: Proxy fieldName
    -- Apply the transformation function using the field name and value
    transformedValue = transform (reflectSymbol fieldProxy) (Record.get fieldProxy record)
    -- Recurse for the rest of the fields in the record
    restTransformation = mapValuesWithIndexBuilder @tail transform record
    -- Insert the transformed field value into the record
    applyTransformation = Builder.insert fieldProxy transformedValue

-- | Base case: Empty record (i.e., no fields to transform).
instance MapValuesWithIndex RL.Nil row a b () () where
  mapValuesWithIndexBuilder _ _ = identity
