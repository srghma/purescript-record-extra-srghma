module Test.Tests.AppendRecordTests where

import Prelude

import Record.ExtraSrghma.AppendRecord (appendRecord)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype AdditiveInt = AdditiveInt Int

derive newtype instance Show AdditiveInt
derive newtype instance Eq AdditiveInt

instance Semigroup AdditiveInt where
  append (AdditiveInt a) (AdditiveInt b) = AdditiveInt (a + b)

spec :: Spec Unit
spec =
  describe "appendRecord" do

    it "appends overlapping subrecords with Semigroup values (String)" do
      let
        bigger = { foo: "a", bar: "b", baz: 42 }
        smaller = { foo: "x", bar: "y" }
        expected = { foo: "ax", bar: "by", baz: 42 }
      appendRecord bigger smaller `shouldEqual` expected

    it "appends with numeric values using addition" do
      let
        bigger = { x: AdditiveInt 10, y: AdditiveInt 5, tag: "done" }
        smaller = { x: AdditiveInt 1, y: AdditiveInt 2 }
        expected = { x: AdditiveInt 11, y: AdditiveInt 7, tag: "done" }
      appendRecord bigger smaller `shouldEqual` expected

    -- NOTE: will not throw unlike `(bigger <> smaller) `shouldEqual` expected` (Which requires them to be equal)

    it "does nothing when smaller is empty" do
      let
        bigger = { x: "hello", y: "world" }
        smaller = {}
        expected = bigger
      appendRecord bigger smaller `shouldEqual` expected
