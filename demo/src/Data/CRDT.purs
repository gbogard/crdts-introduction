module Data.CRDT (
  -- * Classes
  class StateBasedCRDT,
  mergeStates,
  class Generator,
  generateOperations,
  class Effector,
  applyOperations,
  -- * Data types
  State (..)
) where

import Prelude

import Data.List (List)
import Test.QuickCheck (class Arbitrary)

newtype State t = State t

derive newtype instance eqState :: Eq t => Eq (State t)
derive newtype instance showState :: Show t => Show (State t)
derive newtype instance arbitraryState :: Arbitrary t => Arbitrary (State t)

class StateBasedCRDT t where
  mergeStates :: State t -> State t -> State t

class Generator t op | t -> op where
  generateOperations :: State t -> State t -> List op

class Effector t op | t -> op where
  applyOperations :: List op -> State t -> State t