module Test.Tests.FoldrValuesSpec where

import Prelude

import Record.ExtraSrghma (foldrValues)
import Test.Spec.Assertions (shouldEqual)

import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "FoldrValuesTests" do
  it "foldrValues" do
    foldrValues (+) 0 { a: 1, b: 2, c: 3 } `shouldEqual` 6
