module Test.Tests.FoldrValuesLazyTests where

import Prelude

import Record.ExtraSrghma (foldrValuesLazy)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "FoldrValuesLazyTests" do
  it "foldrValuesLazy" do
    let
      record = { a: \_ -> 1, b: \_ -> 2, c: \_ -> 3 }
      result = foldrValuesLazy (\field accum -> \_ -> field unit + accum unit) (\_ -> 0) record
    result unit `shouldEqual` 6
