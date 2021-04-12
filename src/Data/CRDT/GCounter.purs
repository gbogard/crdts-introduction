module Data.CRDT.GCounter
  ( GCounter
  , query
  , increment
  , toMap
  ) where

import Prelude

import Data.CRDT (class StateBasedCRDT)
import Data.Foldable (sum)
import Data.HashMap as Map
import Data.Ord (greaterThanOrEq)
import Data.Replication (ReplicaId)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, suchThat)

-- | A state-based grow-only counter
newtype GCounter
  = GCounter (Map.HashMap ReplicaId Int)

derive newtype instance gCounterEq :: Eq GCounter

derive newtype instance gCounterShow :: Show GCounter

instance gCounterArbitrary :: Arbitrary GCounter where
  arbitrary = GCounter <<< Map.fromArray <$> arrayOf kvGen
    where
    kvGen :: Gen (Tuple ReplicaId Int)
    kvGen = Tuple <$> arbitrary <*> suchThat arbitrary (\x -> greaterThanOrEq x 0)
    

instance gCounterStateBasedCRDT :: StateBasedCRDT GCounter

instance gCounterSemigroup :: Semigroup GCounter where
  append (GCounter a) (GCounter b) = GCounter $ Map.unionWith max a b

instance gCounterMonoid :: Monoid GCounter where
  mempty = GCounter Map.empty

query :: GCounter -> Int
query (GCounter map) = sum map

increment :: ReplicaId -> GCounter -> GCounter
increment replica (GCounter map) = GCounter $ Map.insertWith add replica 1 map

toMap :: GCounter -> Map.HashMap ReplicaId Int
toMap (GCounter map) = map
