module Data.CRDT.OpCounter where

import Prelude

import Data.CRDT (class Effector, class Generator)
import Test.QuickCheck (class Arbitrary)

-- | An operation based counter that supports a single 'Add' operation, with an ineger that can
-- | positive or negative.
newtype OpCounter = OpCounter Int

derive newtype instance showOpCounter :: Show OpCounter
derive newtype instance eqOpCounter :: Eq OpCounter
derive newtype instance arbitraryOpCounter :: Arbitrary OpCounter

newtype OpCounterOperation = Add Int

derive newtype instance showOpCounterOperation :: Show OpCounterOperation
derive newtype instance eqOpCounterOperation :: Eq OpCounterOperation
derive newtype instance arbitraryOpCounterOperation :: Arbitrary OpCounterOperation

instance opCounterGenerator :: Generator OpCounter OpCounterOperation where
  generateOperations (OpCounter a) (OpCounter b) = [(Add (b - a))]

instance opCounterEffector :: Effector OpCounter OpCounterOperation where
  applyOperation {data: OpCounter currentState} {data: Add int} = OpCounter $ currentState + int