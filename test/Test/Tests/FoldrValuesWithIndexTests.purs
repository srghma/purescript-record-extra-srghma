module Test.Tests.FoldrValuesWithIndexTests where

import Prelude

import Record.ExtraSrghma (foldrValuesWithIndex)
import Test.Spec.Assertions (shouldEqual)

import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "FoldrValuesWithIndexTests" do
  it "foldrValuesWithIndex" do
    foldrValuesWithIndex (\key val acc -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 }
      `shouldEqual` "c3b2a1"
