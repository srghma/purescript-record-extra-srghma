module Test.Tests.SListSpec where

import Prelude
import Record.ExtraSrghma.SList.Array
import Record.ExtraSrghma.SList.List
import Record.ExtraSrghma.SList.Types

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
    it "slistToLS" do
      (slistToLS @SNil) `shouldEqual` List.fromFoldable []
      (slistToLS @("asdf" ::: SNil)) `shouldEqual` List.fromFoldable [ "asdf" ]
      (slistToLS @(SCons "asdf" SNil)) `shouldEqual` List.fromFoldable [ "asdf" ]
      (slistToLS @MySlist) `shouldEqual` List.fromFoldable [ "asdf" ]
      (slistToLS @MySlist2) `shouldEqual` List.fromFoldable [ "asdf" ]

    it "slistToAS" do
      (slistToAS @SNil) `shouldEqual` []
      (slistToAS @("asdf" ::: SNil)) `shouldEqual` [ "asdf" ]
      (slistToAS @(SCons "asdf" SNil)) `shouldEqual` [ "asdf" ]
      (slistToAS @MySlist) `shouldEqual` [ "asdf" ]
      (slistToAS @MySlist2) `shouldEqual` [ "asdf" ]

    it "slistToNELS" do
      (slistToNELS @("asdf" ::: SNil)) `shouldEqual` ("asdf" :| List.Nil)

    it "slistToNEAS" do
      (slistToNEAS @("asdf" ::: SNil)) `shouldEqual` NonEmptyArray.cons' "asdf" []

    it "slistToLNES" do
      (slistToLNES @("asdf" ::: SNil)) `shouldEqual` List.fromFoldable [ nes (Proxy :: Proxy "asdf") ]

    it "slistToNELNES" do
      (slistToNELNES @("asdf" ::: SNil)) `shouldEqual` (nes (Proxy :: Proxy "asdf") :| List.Nil)

    it "slistToANES" do
      (slistToANES @("asdf" ::: SNil)) `shouldEqual` [ nes (Proxy :: Proxy "asdf") ]

    it "slistToNEANES" do
      (slistToNEANES @("asdf" ::: SNil)) `shouldEqual` NonEmptyArray.cons' (nes (Proxy :: Proxy "asdf")) []
