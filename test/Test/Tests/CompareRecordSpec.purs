module Test.Tests.CompareRecordSpec where

import Prelude

import Record.ExtraSrghma (compareRecord)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "CompareRecordTests" do
  it "compares two records deterministically" do
    let record1 = { a: 1, b: 10 }
    let record2 = { a: 1, b: 20 }
    let record3 = { a: 2, b: 5 }

    compareRecord record1 record2 `shouldEqual` LT
    compareRecord record2 record1 `shouldEqual` GT
    compareRecord record1 record1 `shouldEqual` EQ
    compareRecord record1 record3 `shouldEqual` LT
    compareRecord record3 record1 `shouldEqual` GT
