module Test.Tests.KeysSpec (spec) where

import Prelude
import Record.ExtraSrghma.Keys.Array (recordToArrayOfKeys, rowToArrayOfKeys)
import Record.ExtraSrghma.Keys.List (recordToListOfKeys, rowToListOfKeys)
import Record.ExtraSrghma.Keys.Pick (pick)

import Data.List as List
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type MyRecord = { bar :: String, baz :: Boolean }

spec :: Spec Unit
spec = describe "KeysSpec" do
  it "rowToListOfKeys" do
    rowToListOfKeys @(bar :: String, baz :: Boolean) `shouldEqual` List.fromFoldable [ "bar", "baz" ]

  it "recordToListOfKeys" do
    recordToListOfKeys @MyRecord `shouldEqual` List.fromFoldable [ "bar", "baz" ]

  it "rowToArrayOfKeys" do
    rowToArrayOfKeys @(bar :: String, baz :: Boolean) `shouldEqual` [ "bar", "baz" ]

  it "recordToArrayOfKeys" do
    recordToArrayOfKeys @MyRecord `shouldEqual` [ "bar", "baz" ]

  it "pick" do
    let testRecord2 = { foo: 1, bar: "x", baz: true }
    (pick testRecord2 :: { bar :: String, baz :: Boolean }) `shouldEqual` { bar: "x", baz: true }
    (pick @(bar :: String, baz :: Boolean) testRecord2) `shouldEqual` { bar: "x", baz: true } -- other way
