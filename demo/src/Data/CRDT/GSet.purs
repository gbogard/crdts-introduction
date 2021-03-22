module Data.CRDT.GSet (
  GSet,
  query,
  insert
) where
  
import Prelude

import Data.CRDT (class StateBasedCRDT, defaultMerge)
import Data.HashSet as Set
import Data.Hashable (class Hashable)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | A simple grow-only set, where the merge function is just a union of the sets
newtype GSet t = GSet (Set.HashSet t)

derive newtype instance eqGSet :: (Eq t) => Eq (GSet t)
derive newtype instance showGSet :: (Show t) =>  Show (GSet t)
derive newtype instance semigroupGSet :: (Hashable t) => Semigroup (GSet t)
derive newtype instance monoidGSet :: (Hashable t) => Monoid (GSet t)

instance arbitraryGSet :: (Arbitrary t, Hashable t) => Arbitrary (GSet t) where
  arbitrary = GSet <<< Set.fromArray <$> arbitrary 

instance stateBasedCRDTGSet :: (Hashable t) => StateBasedCRDT (GSet t) where
  merge = defaultMerge

query :: forall t. GSet t -> Set.HashSet t 
query (GSet set) = set

insert :: forall t. Hashable t => t -> GSet t -> GSet t
insert value (GSet set) = GSet $ Set.insert value set
