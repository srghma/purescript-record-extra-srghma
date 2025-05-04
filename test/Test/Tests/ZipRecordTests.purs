module Test.Tests.ZipRecordTests where

import Prelude

import Data.Tuple (Tuple(..))
import Record.ExtraSrghma (zipRecord)
import Test.Spec.Assertions (shouldEqual)

import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "ZipRecordTests" do
    it "zips two matching records" do
      let
        recordA = { a: 1, b: true }
        recordB = { a: "hello", b: "world" }

        expected = { a: Tuple 1 "hello", b: Tuple true "world" }
        result = zipRecord recordA recordB

      result `shouldEqual` expected
