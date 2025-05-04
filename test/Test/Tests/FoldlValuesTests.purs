module Test.Tests.FoldlValuesTests where

import Prelude

import Record.ExtraSrghma (foldlValues)
import Test.Spec.Assertions (shouldEqual)

import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "FoldlValuesTests" do
  it "foldlValues" do
    foldlValues (+) 0 { a: 1, b: 2, c: 3 } `shouldEqual` 6
