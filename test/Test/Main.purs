module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Discovery (discoverAndRunSpecs)

main :: Effect Unit
main = discoverAndRunSpecs [ consoleReporter ] """Test\.Tests\..*"""
