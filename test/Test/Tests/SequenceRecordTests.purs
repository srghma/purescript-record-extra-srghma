module Test.Tests.SequenceRecordTests where

import Prelude

import Test.Spec (Spec, describe, it)

import Effect.Aff (Aff)
import Test.Spec.Assertions (shouldEqual)

import Record.ExtraSrghma.SequenceRecord (sequenceRecord)

testRecord :: { a :: Aff Int, b :: Aff String }
testRecord =
  { a: pure 1
  , b: pure "hello"
  }

spec :: Spec Unit
spec =
  describe "SequenceRecordTests" do
    it "sequences a record of Affs into an Aff of a record" do
      result <- sequenceRecord testRecord
      result.a `shouldEqual` 1
      result.b `shouldEqual` "hello"
