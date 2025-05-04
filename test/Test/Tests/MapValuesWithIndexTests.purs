module Test.Tests.MapValuesWithIndexTests where

import Prelude

import Record.ExtraSrghma (mapValuesWithIndex)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "MapValuesWithIndexTests" do
    it "mapValuesWithIndex" do
      mapValuesWithIndex (\key val -> key <> show val) { a: 1, b: 2, c: 3 }
        `shouldEqual` { a: "a1", b: "b2", c: "c3" }
