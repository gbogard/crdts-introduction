module Data.CRDT
  ( class StateBasedCRDT
  , merge
  , class Generator
  , generateOperations
  , class Effector
  , applyOperation
  , applyOperations
  ) where

import Prelude
import Data.Foldable (class Foldable, foldr)

-- | A state-based CRDT is a structure that has a binary 'merge' operation that is
-- | * associative
-- | * commutative
-- | * idempotent
-- | and a neutral element that also serves a the initial state for a new replica
-- | It is essentialy a commutative and idempotent monoid, also known as a bounded join-semilattice.
-- | If we encode the state-based CRDT as an empty class whose instances are already monoids,
-- | then we can define 'merge' as a synonym for 'append'. The class is just a way of signaling
-- | that 'append' is also commutative and idempotent
class Monoid t <= StateBasedCRDT t

merge :: forall t. StateBasedCRDT t => t -> t -> t
merge = append

-- | The Generator is the first part of an operation-based CRDT. It's a way of expressing a
-- | user-performed data modification as an operation that can be serialised and sent over the network
class Generator t op | t -> op where
  generateOperations :: t -> t -> Array op

-- | The Effector is the second part of an operation-based CRDT. It's a way of applying an operation
-- | to a current state to produce a new state.
class Effector t op | t -> op where
  applyOperation :: op -> t -> t

applyOperations ::
  forall t op f. Effector t op => Foldable f => f op -> t -> t
applyOperations = flip $ foldr applyOperation
