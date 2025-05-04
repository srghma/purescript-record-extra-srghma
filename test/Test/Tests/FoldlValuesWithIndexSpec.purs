module Test.Tests.FoldlValuesWithIndexSpec where

import Prelude

import Record.ExtraSrghma (foldlValuesWithIndex)
import Record.ExtraSrghma.FoldlValuesWithIndex (foldlValuesWithIndex1)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "FoldlValuesWithIndexTests" do
  it "foldlValuesWithIndex" do
    foldlValuesWithIndex (\acc key val -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 }
      `shouldEqual` "a1b2c3"

  it "foldlValuesWithIndex1 FIXME!!!!" do
    foldlValuesWithIndex1 (\acc key val -> acc <> key <> val) { a: "1", b: "2", c: "3" }
      `shouldEqual` "1b2c3" -- TODO: fix
