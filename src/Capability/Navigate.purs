module Capability.Navigate where

import Prelude
import Data.Route (Route)
import Halogen (HalogenM, lift)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots output m) where
  navigate = lift <<< navigate