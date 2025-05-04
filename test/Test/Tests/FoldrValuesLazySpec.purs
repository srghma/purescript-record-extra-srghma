module Test.Tests.FoldrValuesLazySpec where

import Prelude

import Record.ExtraSrghma (foldrValuesLazy)
import Record.ExtraSrghma.FoldrValuesLazy (foldrValuesLazy1)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "FoldrValuesLazyTests" do
  it "foldrValuesLazy" do
    let
      record = { a: \_ -> 1, b: \_ -> 2, c: \_ -> 3 }
      result = foldrValuesLazy (\field accum -> \_ -> field unit + accum unit) (\_ -> 0) record
    result unit `shouldEqual` 6
  it "foldrValuesLazy1" do
    let
      record = { a: \_ -> 1, b: \_ -> 2, c: \_ -> 3 }
      result = foldrValuesLazy1 (\field accum -> \_ -> field unit + accum unit) record
    result unit `shouldEqual` 6
