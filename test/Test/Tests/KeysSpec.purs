module Test.Tests.KeysSpec where

import Prelude
import Record.ExtraSrghma.Keys (pick, recordKeys, recordKeys', rowKeys, rowKeys', slistKeys, slistKeys')

import Data.List (List)
import Data.List as List
import Record.ExtraSrghma (type (:::), SCons, SNil)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

type MySlist = SCons "asdf" SNil
type MySlist2 = "asdf" ::: SNil

type MyRecord = { bar :: String, baz :: Boolean }

spec :: Spec Unit
spec =
  describe "KeysTests" do
    it "rowKeys" do
      let result = List.sort (rowKeys @(bar :: String, baz :: Boolean)) :: List String
      result `shouldEqual` List.fromFoldable [ "bar", "baz" ]

    it "rowKeys'" do
      let result = List.sort (rowKeys' (Proxy :: Proxy (bar :: String, baz :: Boolean))) :: List String
      result `shouldEqual` List.fromFoldable [ "bar", "baz" ]

    it "recordKeys" do
      let result = List.sort (recordKeys @MyRecord) :: List String
      result `shouldEqual` List.fromFoldable [ "bar", "baz" ]

    it "recordKeys'" do
      let result = List.sort (recordKeys' (Proxy :: _ MyRecord)) :: List String
      result `shouldEqual` List.fromFoldable [ "bar", "baz" ]

    it "pick" do
      let testRecord2 = { foo: 1, bar: "x", baz: true }

      let selected = pick testRecord2 :: { bar :: String, baz :: Boolean }
      selected `shouldEqual` { bar: "x", baz: true }

      let selected2 = pick @(bar :: String, baz :: Boolean) testRecord2
      selected2 `shouldEqual` { bar: "x", baz: true }

    it "slistKeys" do
      (List.sort $ slistKeys @SNil) `shouldEqual` List.fromFoldable []
      -- purs: An internal error occurred during compilation: inferKind: Unimplemented case
      -- "asdf" Record.ExtraSrghma.SList.::: SNil
      --
      -- Please report this at https://github.com/purescript/purescript/issues
      -- CallStack (from HasCallStack):
      --   error, called at src/Language/PureScript/Crash.hs:10:3 in purescript-0.15.15-A1Hsm5jzdq3K6f9tg0KeFN:Language.PureScript.Crash
      --   internalError, called at src/Language/PureScript/TypeChecker/Kinds.hs:242:7 in purescript-0.15.15-A1Hsm5jzdq3K6f9tg0KeFN:Language.PureScript.TypeChecker.Kinds
      --   inferKind, called at src/Language/PureScript/TypeChecker/Kinds.hs:327:25 in purescript-0.15.15-A1Hsm5jzdq3K6f9tg0KeFN:Language.PureScript.TypeChecker.Kinds
      --   checkKind', called at src/Language/PureScript/TypeChecker/Kinds.hs:303:13 in purescript-0.15.15-A1Hsm5jzdq3K6f9tg0KeFN:Language.PureScript.TypeChecker.Kinds
      --   checkKind, called at src/Language/PureScript/TypeChecker/Types.hs:476:45 in purescript-0.15.15-A1Hsm5jzdq3K6f9tg0KeFN:Language.PureScript.TypeChecker.Types
      --            Src   Lib   All
      -- (List.sort $ slistKeys @("asdf" ::: SNil)) `shouldEqual` List.fromFoldable []
      (List.sort $ slistKeys @(SCons "asdf" SNil)) `shouldEqual` List.fromFoldable [ "asdf" ]
      (List.sort $ slistKeys @MySlist) `shouldEqual` List.fromFoldable [ "asdf" ]
      (List.sort $ slistKeys @MySlist2) `shouldEqual` List.fromFoldable [ "asdf" ]

    it "slistKeys'" do
      (List.sort $ slistKeys' (Proxy :: _ SNil)) `shouldEqual` List.fromFoldable []
      (List.sort $ slistKeys' (Proxy :: _ ("asdf" ::: SNil))) `shouldEqual` List.fromFoldable [ "asdf" ]
      (List.sort $ slistKeys' (Proxy :: _ (SCons "asdf" SNil))) `shouldEqual` List.fromFoldable [ "asdf" ]
      (List.sort $ slistKeys' (Proxy :: _ MySlist)) `shouldEqual` List.fromFoldable [ "asdf" ]
      (List.sort $ slistKeys' (Proxy :: _ MySlist2)) `shouldEqual` List.fromFoldable [ "asdf" ]
