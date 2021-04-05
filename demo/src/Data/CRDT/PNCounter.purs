module Data.CRDT.PNCounter (
  PNCounter,
  query,
  increment,
  decrement
) where
  
import Prelude

import Data.CRDT (class StateBasedCRDT)
import Data.CRDT.GCounter as GCounter
import Data.Replication (ReplicaId)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | A counter that supports increments and decrements
data PNCounter = PNCounter GCounter.GCounter GCounter.GCounter

derive instance eqPNCounter :: Eq PNCounter

instance semigroupPNCounter :: Semigroup PNCounter where
 append (PNCounter a b) (PNCounter a' b') = PNCounter (a <> a') (b <> b')

instance monoidPNCounter :: Monoid PNCounter where
  mempty = PNCounter mempty mempty

instance arbitraryPNCounter :: Arbitrary PNCounter where
  arbitrary = PNCounter <$> arbitrary <*> arbitrary

instance stateBasedCRDTGSet :: StateBasedCRDT PNCounter

query :: PNCounter -> Int
query (PNCounter a b) = GCounter.query a - GCounter.query b

increment :: ReplicaId -> PNCounter -> PNCounter
increment rid (PNCounter a b) = PNCounter (GCounter.increment rid a) b

decrement :: ReplicaId -> PNCounter -> PNCounter
decrement rid (PNCounter a b) = PNCounter a (GCounter.increment rid b)
