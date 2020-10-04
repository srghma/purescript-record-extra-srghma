module Test.Main where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Timer (setTimeout)
import Record.Extra (mapRecord)
import Record.ExtraSrghma (mapIndex, mapValuesWithIndex, parSequenceRecord)
import Test.Assert (assert')
import Type.Prelude (RProxy(..))

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
  assert' "mapIndex" $
    mapIndex (\key -> key) (RProxy :: forall type_ . RProxy (WithoutVals type_)) == { a: "a", b: "b" }

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

