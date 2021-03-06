module Test.Main where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Timer (setTimeout)
import Record.ExtraSrghma (foldlValues, foldlValuesWithIndex, foldrValues, foldrValuesLazy, foldrValuesWithIndex, mapIndex, mapRecord, mapValuesWithIndex, parSequenceRecord, valuesToUnfoldableLazy)
import Test.Assert (assert')
import Type.Prelude (Proxy(..))
import Data.List as List

type WithoutVals :: forall k. k -> Row k
type WithoutVals type_ =
  ( a :: type_
  , b :: type_
  )

type Reqs type_ =
  { a :: type_
  , b :: type_
  }

main :: Effect Unit
main = do
  assert' "foldlValues" $
    foldlValues (+) 0 { a: 1, b: 2, c: 3 } == 6

  assert' "foldlValuesWithIndex" $
    foldlValuesWithIndex (\acc key val -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 } == "a1b2c3"

  assert' "foldrValues" $
    foldrValues (+) 0 {a: 1, b: 2, c: 3} == 6

  assert' "foldrValuesLazy" $
    let
      record = {a: \_ -> 1, b: \_ -> 2, c: \_ -> 3}
      (result :: Unit -> Int) = foldrValuesLazy (\field accum -> \_ -> field unit + accum unit) (\_ -> 0) record
     in result unit == 6

  assert' "foldrValuesWithIndex" $
    foldrValuesWithIndex (\key val acc -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 } == "c3b2a1"

  assert' "valuesToUnfoldableLazy" $
    valuesToUnfoldableLazy {a: 1, b: 2, c: 3} == List.fromFoldable [1, 2, 3]

  assert' "mapIndex" $
    mapIndex (\key -> key) (Proxy :: forall type_ . Proxy (WithoutVals type_)) == { a: "a", b: "b" }

  assert' "mapValuesWithIndex" $
    mapValuesWithIndex (\key val -> key <> show val) {a: 1, b: 2, c: 3} == { a: "a1", b: "b2", c: "c3" }

  let
    (parSequenceRecord__config :: Reqs String) =
      { a: "www.purescript.org"
      , b: "try.purescript.org"
      }

    parSequenceRecord__process input = ContT \handler -> void $ setTimeout 0 $ handler $ input <> " success"

    (parSequenceRecord__config' :: Reqs (ContT Unit Effect String)) = mapRecord parSequenceRecord__process parSequenceRecord__config
    (parSequenceRecord__config'' :: ContT Unit Effect (Reqs String)) = parSequenceRecord parSequenceRecord__config'

  runContT parSequenceRecord__config'' \(result :: Reqs String) -> do
    assert' "parSequenceRecord" $ result ==
      { a: "www.purescript.org success"
      , b: "try.purescript.org success"
      }
    logShow "async test parSequenceRecord finished"

  logShow "tests finished"
