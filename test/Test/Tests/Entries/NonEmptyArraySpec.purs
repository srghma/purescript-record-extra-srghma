module Test.Tests.Entries.NonEmptyArraySpec where

import Prelude
import Record.ExtraSrghma.Entries.NonEmptyArray (recordToNEAKV, recordToNEANEKV)

import Data.Array.NonEmpty as NEA
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

spec :: Spec Unit
spec = do
  describe "recordToNEAKV" do

    it "converts a singleton record to a NonEmptyArray of (String, Int)" do
      let rec = { a: 42 } :: Record (a :: Int)
      recordToNEAKV rec `shouldEqual` NEA.singleton (Tuple "a" 42)

    it "converts a multi-field record to a NonEmptyArray of (String, Int)" do
      let
        rec = { b: 1, a: 2, c: 3 } :: Record (a :: Int, b :: Int, c :: Int)
        result = recordToNEAKV rec
        expectedKeys = [ "a", "b", "c" ]
        actualKeys = NEA.toArray result <#> \(Tuple k _) -> k
      actualKeys `shouldEqual` expectedKeys

  describe "recordToNEANEKV" do

    it "converts a singleton record to a NonEmptyArray of (NonEmptyString, Int)" do
      let
        rec = { foo: 99 } :: Record (foo :: Int)
        expectedKey = NES.nes (Proxy :: Proxy "foo")
      recordToNEANEKV rec `shouldEqual` NEA.singleton (Tuple expectedKey 99)

    it "converts a multi-field record to a NonEmptyArray of (NonEmptyString, Int)" do
      recordToNEANEKV { x: 5, y: 10 } `shouldEqual` NEA.cons' (Tuple (NES.nes (Proxy :: _ "x")) 5) [ Tuple (NES.nes (Proxy :: _ "y")) 10 ]
