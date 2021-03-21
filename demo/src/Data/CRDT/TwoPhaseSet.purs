module CRDT.TwoPhaseSet (
  TwoPhaseSet,
  insert,
  remove,
  query
) where
  
import Prelude

import Data.CRDT (class StateBasedCRDT, State(..))
import Data.HashSet as HashSet
import Data.HashSet as Set
import Data.Hashable (class Hashable)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | A set that supports adding elements, or removing it definitively 
-- | Removed elements are kept around in a special *tombstone* set.
data TwoPhaseSet t = TwoPhaseSet (Set.HashSet t) (Set.HashSet t)

derive instance eqTwoPhaseSet :: (Eq t) => Eq (TwoPhaseSet t)

instance showTwoPhaseSet :: (Hashable t, Show t) => Show (TwoPhaseSet t) where
  show = show <<< query

instance semigroupTwoPhaseSet :: (Hashable t) => Semigroup (TwoPhaseSet t) where
  append (TwoPhaseSet a b) (TwoPhaseSet a' b') = TwoPhaseSet (a <> a') (b <> b')

instance monoidTwoPhaseSet :: (Hashable t) => Monoid (TwoPhaseSet t) where
  mempty = TwoPhaseSet mempty mempty

instance stateBasedCRDTTwoPhaseSet :: (Hashable t) => StateBasedCRDT (TwoPhaseSet t) where
  mergeStates (State a) (State b) = State $ a <> b

instance arbitraryTwoPhaseSet :: (Arbitrary t, Hashable t) => Arbitrary (TwoPhaseSet t) where
  arbitrary = do
    a <- HashSet.fromArray <$> arbitrary
    b <- HashSet.fromArray <$> arbitrary
    pure $ TwoPhaseSet a b

insert :: forall t. Hashable t => t -> TwoPhaseSet t -> TwoPhaseSet t
insert value (TwoPhaseSet a b) = TwoPhaseSet (HashSet.insert value a) b 

-- | Removes an element from the set
-- | Note that an element, once removed, cannot be inserted back
remove :: forall t. Hashable t => t -> TwoPhaseSet t -> TwoPhaseSet t
remove value (TwoPhaseSet a b) = TwoPhaseSet a $ HashSet.insert value b

query :: forall t. Hashable t => TwoPhaseSet t -> Set.HashSet t
query (TwoPhaseSet a b) = HashSet.difference a b
