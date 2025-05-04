module Test.Tests.FoldrValuesSpec where

import Prelude

import Record.ExtraSrghma (foldrValues)
import Record.ExtraSrghma.FoldrValues (foldrValues1)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "FoldrValuesTests" do
  it "foldrValues" do
    foldrValues (+) 0 { a: 1, b: 2, c: 3 } `shouldEqual` 6
  it "foldrValues1" do
    foldrValues1 (+) { a: 1, b: 2, c: 3 } `shouldEqual` 6
