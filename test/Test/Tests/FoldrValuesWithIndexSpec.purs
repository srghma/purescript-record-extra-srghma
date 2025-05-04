module Test.Tests.FoldrValuesWithIndexSpec where

import Prelude

import Record.ExtraSrghma (foldrValuesWithIndex)
import Record.ExtraSrghma.FoldrValuesWithIndex (foldrValuesWithIndex1)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "FoldrValuesWithIndexTests" do
  it "foldrValuesWithIndex" do
    foldrValuesWithIndex (\key val acc -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 }
      `shouldEqual` "c3b2a1"
  it "foldrValuesWithIndex1 FIXME!!!!" do
    foldrValuesWithIndex1 (\key val acc -> acc <> key <> val) { a: "1", b: "2", c: "3" }
      `shouldEqual` "1c3b2" -- TODO: FIXME!!!!
