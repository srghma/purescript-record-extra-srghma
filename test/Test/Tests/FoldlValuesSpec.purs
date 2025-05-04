module Test.Tests.FoldlValuesSpec where

import Prelude

import Record.ExtraSrghma (foldlValues)
import Record.ExtraSrghma.FoldlValues (foldlValues1)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "FoldlValuesTests" do
  it "foldlValues" do
    foldlValues (+) 0 { a: 1, b: 2, c: 3 } `shouldEqual` 6

  it "foldlValues1" do
    foldlValues1 (+) { a: 1, b: 2, c: 3 } `shouldEqual` 6
