module Test.Tests.KeysSpec (spec) where

import Prelude
import Record.ExtraSrghma.Keys.Array (recordToAK, rowToAK)
import Record.ExtraSrghma.Keys.List (recordToLK, rowToLK, rowToLNEK)
import Record.ExtraSrghma.Keys.NonEmptyList (rowToNELK, rowToNELNEK)

import Data.List as List
import Data.NonEmpty ((:|))
import Data.String.NonEmpty (nes)
import Record.ExtraSrghma.Keys.Pick (pick)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

type MyRecord = { bar :: String, baz :: Boolean }

spec :: Spec Unit
spec = describe "KeysSpec" do
  it "rowToLK" do
    rowToLK @(bar :: String, baz :: Boolean) `shouldEqual` List.fromFoldable [ "bar", "baz" ]

  it "rowToLNEK" do
    rowToLNEK @(bar :: String, baz :: Boolean) `shouldEqual` List.fromFoldable [ nes (Proxy :: _ "bar"), nes (Proxy :: _ "baz") ]
    rowToLNEK @() `shouldEqual` List.fromFoldable []

  it "rowToNELK" do
    rowToNELK @(bar :: String, baz :: Boolean) `shouldEqual` ("bar" :| List.fromFoldable [ "baz" ])

  it "rowToNELNEK" do
    rowToNELNEK @(bar :: String, baz :: Boolean) `shouldEqual` ((nes (Proxy :: _ "bar")) :| List.fromFoldable [ (nes (Proxy :: _ "baz")) ])
  -- throws
  --  rowToNELNEK @("" :: String, baz :: Boolean) `shouldEqual` ((nes (Proxy :: _ "bar")) :| List.fromFoldable [ (nes (Proxy :: _ "baz")) ])

  it "recordToLK" do
    recordToLK @MyRecord `shouldEqual` List.fromFoldable [ "bar", "baz" ]

  it "rowToAK" do
    rowToAK @(bar :: String, baz :: Boolean) `shouldEqual` [ "bar", "baz" ]

  it "recordToAK" do
    recordToAK @MyRecord `shouldEqual` [ "bar", "baz" ]

  it "pick" do
    let testRecord2 = { foo: 1, bar: "x", baz: true }
    (pick testRecord2 :: { bar :: String, baz :: Boolean }) `shouldEqual` { bar: "x", baz: true }
    (pick @(bar :: String, baz :: Boolean) testRecord2) `shouldEqual` { bar: "x", baz: true } -- other way
