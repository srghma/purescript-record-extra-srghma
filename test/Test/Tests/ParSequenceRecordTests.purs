module Test.Tests.ParSequenceRecordTests where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (setTimeout)
import Record.ExtraSrghma (mapRecord, parSequenceRecord)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec (Spec, describe, it)

type Reqs type_ =
  { a :: type_
  , b :: type_
  }

spec :: Spec Unit
spec =
  describe "ParSequenceRecordTests" do
    it "works" do
      let
        config :: Reqs String
        config =
          { a: "www.purescript.org"
          , b: "try.purescript.org"
          }

        process input = ContT \handler -> void $ setTimeout 0 $ handler $ input <> " success"

        config' :: Reqs (ContT Unit Effect String)
        config' = mapRecord process config

        runAll :: ContT Unit Effect (Reqs String)
        runAll = parSequenceRecord config'

      liftEffect $ runContT runAll \result -> do
        result `shouldEqual`
          { a: "www.purescript.org success"
          , b: "try.purescript.org success"
          }
