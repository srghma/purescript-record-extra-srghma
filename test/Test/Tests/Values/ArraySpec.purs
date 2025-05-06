module Test.Tests.Values.ArraySpec where

import Prelude

import Data.Array as Array
import Record.ExtraSrghma.Values.Array (recordToAV)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "recordToAV" do
  it "converts an empty record to an empty array" do
    recordToAV {} `shouldEqual` ([] :: Array Int)

  it "converts a singleton record to a single-element array" do
    let rec = { foo: 1 } :: Record (foo :: Int)
    recordToAV rec `shouldEqual` [ 1 ]

  it "converts a record with multiple Int fields into an array of values" do
    let rec = { a: 1, b: 2, c: 3 } :: Record (a :: Int, b :: Int, c :: Int)
    Array.sort (recordToAV rec) `shouldEqual` [ 1, 2, 3 ]

  it "works for homogeneous String fields" do
    let rec = { x: "a", y: "b" } :: Record (x :: String, y :: String)
    Array.sort (recordToAV rec) `shouldEqual` [ "a", "b" ]

  it "fails to compile on heterogeneous records (as expected)" do
    -- Uncommenting the following line should cause a type error (expected behavior):
    -- let x = recordToAV { x: "a", y: 1 }
    pure unit
