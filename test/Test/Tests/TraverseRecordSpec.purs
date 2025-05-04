module Test.Tests.TraverseRecordSpec where

import Prelude

import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import Record.ExtraSrghma.TraverseRecord (traverseRecord)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testRecord :: { a :: Aff Int, b :: Aff String }
testRecord =
  { a: pure 1
  , b: pure "hello"
  }

spec :: Spec Unit
spec =
  describe "TraverseRecord" do
    it "traverses and transforms each field" do
      let inputs = { a: 1, b: "asdf" }
      let
        fns =
          { a: \x -> logShow x *> pure (x + 1)
          , b: \s -> logShow s *> pure (s <> "!")
          }
      result <- traverseRecord fns inputs
      result.a `shouldEqual` 2
      result.b `shouldEqual` "asdf!"
