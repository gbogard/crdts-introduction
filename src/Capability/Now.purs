module Capability.Now where

import Prelude

import Data.DateTime.Instant (Instant)

class Monad m <= MonadNow m where
  now :: m Instant