module Test.Tests.SListSpec where

import Prelude
import Record.ExtraSrghma.SList (type (:::), SCons, SNil, slistToArrayOfNonEmptyStrings, slistToArrayOfNonEmptyStrings1, slistToArrayOfStrings, slistToArrayOfStrings1, slistToListOfNonEmptyStrings, slistToListOfNonEmptyStrings1, slistToListOfStrings, slistToListOfStrings1)

import Data.Array.NonEmpty as NonEmptyArray
import Data.List as List
import Data.NonEmpty ((:|))
import Data.String.NonEmpty (nes)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

type MySlist = SCons "asdf" SNil
type MySlist2 = "asdf" ::: SNil

type MyRecord = { bar :: String, baz :: Boolean }

spec :: Spec Unit
spec =
  describe "KeysTests" do
    it "slistToListOfStrings" do
      (slistToListOfStrings @SNil) `shouldEqual` List.fromFoldable []
      (slistToListOfStrings @("asdf" ::: SNil)) `shouldEqual` List.fromFoldable [ "asdf" ]
      (slistToListOfStrings @(SCons "asdf" SNil)) `shouldEqual` List.fromFoldable [ "asdf" ]
      (slistToListOfStrings @MySlist) `shouldEqual` List.fromFoldable [ "asdf" ]
      (slistToListOfStrings @MySlist2) `shouldEqual` List.fromFoldable [ "asdf" ]

    it "slistToArrayOfStrings" do
      (slistToArrayOfStrings @SNil) `shouldEqual` []
      (slistToArrayOfStrings @("asdf" ::: SNil)) `shouldEqual` [ "asdf" ]
      (slistToArrayOfStrings @(SCons "asdf" SNil)) `shouldEqual` [ "asdf" ]
      (slistToArrayOfStrings @MySlist) `shouldEqual` [ "asdf" ]
      (slistToArrayOfStrings @MySlist2) `shouldEqual` [ "asdf" ]

    it "slistToListOfStrings1" do
      (slistToListOfStrings1 @("asdf" ::: SNil)) `shouldEqual` ("asdf" :| List.Nil)

    it "slistToArrayOfStrings1" do
      (slistToArrayOfStrings1 @("asdf" ::: SNil)) `shouldEqual` NonEmptyArray.cons' "asdf" []

    it "slistToListOfNonEmptyStrings" do
      (slistToListOfNonEmptyStrings @("asdf" ::: SNil)) `shouldEqual` List.fromFoldable [ nes (Proxy :: Proxy "asdf") ]

    it "slistToListOfNonEmptyStrings1" do
      (slistToListOfNonEmptyStrings1 @("asdf" ::: SNil)) `shouldEqual` (nes (Proxy :: Proxy "asdf") :| List.Nil)

    it "slistToArrayOfNonEmptyStrings" do
      (slistToArrayOfNonEmptyStrings @("asdf" ::: SNil)) `shouldEqual` [ nes (Proxy :: Proxy "asdf") ]

    it "slistToArrayOfNonEmptyStrings1" do
      (slistToArrayOfNonEmptyStrings1 @("asdf" ::: SNil)) `shouldEqual` NonEmptyArray.cons' (nes (Proxy :: Proxy "asdf")) []
