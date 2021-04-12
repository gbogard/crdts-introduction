module Data.Route where

import Data.Generic.Rep (class Generic)
import Prelude

data DemoType
  = GSet
  | TwoPhaseSet
  | GCounter

derive instance genericDemoType :: Generic DemoType _

derive instance eqDemoType :: Eq DemoType

instance showDemoType :: Show DemoType where
  show GSet = "gset"
  show TwoPhaseSet = "2pset"
  show GCounter = "gcounter"

data Route
  = HomePage
  | Demo DemoType

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = case _ of
    HomePage -> "/"
    Demo demoType -> "/demo/" <> show demoType
