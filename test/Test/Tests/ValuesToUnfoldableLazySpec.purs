module Test.Tests.ValuesToUnfoldableLazySpec where

import Prelude

import Data.Array as Array
import Data.List as List
import Record.ExtraSrghma (valuesToUnfoldableLazy)
import Test.Spec.Assertions (shouldEqual)

import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "ValuesToUnfoldableLazyTests" do
    let
      testRecord :: { a :: Int, b :: Int, c :: Int }
      testRecord = { a: 1, b: 2, c: 3 }

    it "converts record values to Array" do
      let result = valuesToUnfoldableLazy testRecord :: Array Int
      Array.sort result `shouldEqual` [ 1, 2, 3 ]

    it "converts record values to List" do
      let result = valuesToUnfoldableLazy testRecord :: List.List Int
      List.sort result `shouldEqual` List.fromFoldable [ 1, 2, 3 ]
