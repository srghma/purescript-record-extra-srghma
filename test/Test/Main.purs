module Test.Main where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Data.List as List
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Record.ExtraSrghma (foldlValues, foldlValuesWithIndex, foldrValues, foldrValuesLazy, foldrValuesWithIndex, mapIndex, mapRecord, mapValuesWithIndex, parSequenceRecord, valuesToUnfoldableLazy, zipRecord)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Type.Prelude (Proxy(..))

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
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Record.ExtraSrghma" do
    it "foldlValues" do
      foldlValues (+) 0 { a: 1, b: 2, c: 3 } `shouldEqual` 6

    it "foldlValuesWithIndex" do
      foldlValuesWithIndex (\acc key val -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 }
        `shouldEqual` "a1b2c3"

    it "foldrValues" do
      foldrValues (+) 0 { a: 1, b: 2, c: 3 } `shouldEqual` 6

    it "foldrValuesLazy" do
      let
        record = { a: \_ -> 1, b: \_ -> 2, c: \_ -> 3 }
        result = foldrValuesLazy (\field accum -> \_ -> field unit + accum unit) (\_ -> 0) record
      result unit `shouldEqual` 6

    it "foldrValuesWithIndex" do
      foldrValuesWithIndex (\key val acc -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 }
        `shouldEqual` "c3b2a1"

    it "valuesToUnfoldableLazy" do
      valuesToUnfoldableLazy { a: 1, b: 2, c: 3 }
        `shouldEqual` List.fromFoldable [ 1, 2, 3 ]

    it "mapIndex" do
      mapIndex (\key -> key) (Proxy :: forall type_. Proxy (WithoutVals type_))
        `shouldEqual` { a: "a", b: "b" }

    it "mapValuesWithIndex" do
      mapValuesWithIndex (\key val -> key <> show val) { a: 1, b: 2, c: 3 }
        `shouldEqual` { a: "a1", b: "b2", c: "c3" }

    it "parSequenceRecord" do
      let
        config :: Reqs String
        config =
          { a: "www.purescript.org"
          , b: "try.purescript.org"
          }

        process input = ContT \handler -> void $ setTimeout 0 $ handler $ input <> " success"

        config' :: Reqs (ContT Unit Effect String)
        config' = mapRecord process config

        runAll :: ContT Unit Effect (Reqs String)
        runAll = parSequenceRecord config'

      liftEffect $ runContT runAll \result -> do
        result `shouldEqual`
          { a: "www.purescript.org success"
          , b: "try.purescript.org success"
          }

    describe "Record.ExtraSrghma.ZipRecord" do
      it "zips two matching records" do
        let
          recordA = { a: 1, b: true }
          recordB = { a: "hello", b: "world" }

          expected = { a: Tuple 1 "hello", b: Tuple true "world" }
          result = zipRecord recordA recordB

        result `shouldEqual` expected
