module Data.Replication.VectorClock where

import Data.Foldable
import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap as Map
import Data.Maybe (fromMaybe)
import Data.Replication (ReplicaId)
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype VectorClock = VectorClock (Map.HashMap ReplicaId Int)

derive newtype instance eqVectorClock :: Eq VectorClock
derive newtype instance showVectorClock :: Show VectorClock

instance semigroupVecorClock :: Semigroup VectorClock where
  append (VectorClock a) (VectorClock b) = VectorClock $ Map.unionWith max a b

instance monoidVectorClock :: Monoid VectorClock where
  mempty = VectorClock Map.empty

instance arbitraryVectorClock :: Arbitrary VectorClock where
  arbitrary = VectorClock <<< Map.fromArray <$> arbitrary

increment :: VectorClock -> ReplicaId -> VectorClock
increment (VectorClock map) r = VectorClock $ Map.insertWith add r 1 map

-- | Returns whether a given event happens before another one
-- | VC(x) denotes the VectorClock of an event x. 
-- | VC(x)(r) denotes the component of that clock for a given ReplicaId r
-- | An event x is casually ordered before an event y iff 
-- | VC(x)(r) <= VC(Y)(r) for all r
-- | There exists at least one component r for which VC(x)(r) < VC(x)(y)
-- Implemented as a single fold, to avoid traversing the clock more than once
happensBefore :: VectorClock -> VectorClock -> Boolean
happensBefore (VectorClock x) (VectorClock y) =
  let
    result = foldlWithIndex go {hasStrictlyInferiorTimestamp: false, isInferiorOrEqual: true} x
    go replicaId acc aTimestamp = 
      let
        bTimestamp = fromMaybe 0 $ Map.lookup replicaId y
        hasStrictlyInferiorTimestamp = acc.hasStrictlyInferiorTimestamp || aTimestamp < bTimestamp
        isInferiorOrEqual = acc.isInferiorOrEqual && aTimestamp <= bTimestamp
      in {hasStrictlyInferiorTimestamp, isInferiorOrEqual}
  in result.hasStrictlyInferiorTimestamp && result.isInferiorOrEqual