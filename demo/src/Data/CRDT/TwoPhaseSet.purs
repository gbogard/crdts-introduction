module Data.CRDT.TwoPhaseSet
  ( TwoPhaseSet
  , insert
  , remove
  , query
  , member
  , addSet
  , tombstoneSet
  ) where

import Prelude
import Data.CRDT (class StateBasedCRDT, merge)
import Data.CRDT.GSet as GSet
import Data.HashSet as Set
import Data.Hashable (class Hashable)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | A set that supports adding elements, or removing it definitively 
-- | Removed elements are kept around in a special *tombstone* set.
data TwoPhaseSet t
  = TwoPhaseSet (GSet.GSet t) (GSet.GSet t)

derive instance eqTwoPhaseSet :: (Eq t) => Eq (TwoPhaseSet t)

instance showTwoPhaseSet :: (Hashable t, Show t) => Show (TwoPhaseSet t) where
  show = show <<< query

instance semigroupTwoPhaseSet :: (Hashable t) => Semigroup (TwoPhaseSet t) where
  append (TwoPhaseSet a b) (TwoPhaseSet a' b') = TwoPhaseSet (a <> a') (b <> b')

instance monoidTwoPhaseSet :: (Hashable t) => Monoid (TwoPhaseSet t) where
  mempty = TwoPhaseSet mempty mempty

instance stateBasedCRDTTwoPhaseSet :: (Hashable t) => StateBasedCRDT (TwoPhaseSet t)

instance arbitraryTwoPhaseSet :: (Arbitrary t, Hashable t) => Arbitrary (TwoPhaseSet t) where
  arbitrary = TwoPhaseSet <$> arbitrary <*> arbitrary

insert :: forall t. Hashable t => t -> TwoPhaseSet t -> TwoPhaseSet t
insert value (TwoPhaseSet a b) = TwoPhaseSet (GSet.insert value a) b

-- | Removes an element from the set
-- | Note that an element, once removed, cannot be inserted back
remove :: forall t. Hashable t => t -> TwoPhaseSet t -> TwoPhaseSet t
remove value (TwoPhaseSet a b) = TwoPhaseSet a $ GSet.insert value b

query :: forall t. Hashable t => TwoPhaseSet t -> Set.HashSet t
query (TwoPhaseSet a b) = Set.difference (GSet.query a) (GSet.query b)

member :: forall t. Hashable t => TwoPhaseSet t -> t -> Boolean
member set el = Set.member el (query set)

-- | Returns the 'add set' of the two-phase set, useful for debugging and visualisation
addSet :: forall t. TwoPhaseSet t -> Set.HashSet t
addSet (TwoPhaseSet a _) = GSet.query a

-- | Returns the 'tombstone set' of the two-phase set, useful for debugging and visualisation
tombstoneSet :: forall t. TwoPhaseSet t -> Set.HashSet t
tombstoneSet (TwoPhaseSet _ b) = GSet.query b