module Data.CRDT.GCounter where

import Prelude

import Data.CRDT (class StateBasedCRDT, State(..))
import Data.Foldable (sum)
import Data.HashMap as Map
import Data.Ord (greaterThanOrEq, lessThanOrEq)
import Data.Replication (ReplicaId)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, suchThat)

-- | A state-based grow-only counter
newtype GCounter = GCounter (Map.HashMap ReplicaId Int)

derive newtype instance gCounterEq :: Eq GCounter
derive newtype instance gCounterShow :: Show GCounter

instance gCounterArbitrary :: Arbitrary GCounter where
  arbitrary = GCounter <<< Map.fromArray <$> arrayOf kvGen 
    where 
      kvGen :: Gen (Tuple ReplicaId Int)
      kvGen = Tuple <$> arbitrary <*> suchThat arbitrary (lessThanOrEq 0)

instance gCounterStateBasedCRDT :: StateBasedCRDT GCounter where
  mergeStates (State (GCounter a)) (State (GCounter b)) =
    State <<< GCounter $ Map.unionWith max a b


empty :: GCounter
empty = GCounter Map.empty

value :: GCounter -> Int
value (GCounter map) = sum map

increment :: ReplicaId -> GCounter -> GCounter
increment replica (GCounter map) = GCounter $ Map.upsert (add 1) replica 1 map