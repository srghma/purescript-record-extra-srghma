module Test.Tests.FoldlValuesWithIndexTests where

import Prelude

import Record.ExtraSrghma (foldlValuesWithIndex)
import Test.Spec.Assertions (shouldEqual)

import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "FoldlValuesWithIndexTests" do
  it "foldlValuesWithIndex" do
    foldlValuesWithIndex (\acc key val -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 }
      `shouldEqual` "a1b2c3"
