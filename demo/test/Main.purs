module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Data.CRDT.GCounter (gCounterSpec)
import Test.Data.CRDT.GSet (gSetSpec)
import Test.Data.CRDT.TwoPhaseSet (twoPhaseSetSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  gSetSpec
  twoPhaseSetSpec
  gCounterSpec