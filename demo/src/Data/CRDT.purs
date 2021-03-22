module Data.CRDT (
  -- * Classes
  class StateBasedCRDT,
  merge,
  class Generator,
  generateOperations,
  class Effector,
  applyOperation,
  -- * Data types
  WithTimestamp (..),
  -- * Functions
  defaultMerge,
  mergeStates,
  applyOperations,
  applyOperationAndIncreaseClock,
  applyOperationsAndIncreaseClock
) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Replication (ReplicaId)
import Data.Replication.VectorClock as VectorClock

type WithTimestamp t = { data :: t, timestamp :: VectorClock.VectorClock }

-- | A state-based CRDT is a structure that has a binary 'merge' operation that is
-- | * associative
-- | * commutative
-- | * idempotent
-- | The merge operation may access the vector clocks of each replica, which is useful to implement
-- | "last-writer wins" semantics, but the merging of these clocks is handled by another function.
-- | See 'mergeStates'
class StateBasedCRDT t where
  merge :: WithTimestamp t -> WithTimestamp t -> t

-- | The Generator is the first part of an operation-based CRDT. It's a way of expressing a
-- | user-performed data modification as an operation that can be serialised and sent over the network
class Generator t op | t -> op where
  generateOperations :: t -> t -> Array op

-- | The Effector is the second part of an operation-based CRDT. It's a way of applying an operation
-- | to a current state to produce a new state.
-- | The operation may access the current vector clock, as well as the vector clock at the time of the operation
-- | which is useful to implement "last-writer wins" semantics, but updating the current vector clock
-- | is handled by another function.
class Effector t op | t -> op where
  applyOperation :: WithTimestamp t -> WithTimestamp op -> t

applyOperationAndIncreaseClock :: 
  forall t op. Effector t op => ReplicaId -> WithTimestamp t -> WithTimestamp op -> WithTimestamp t
applyOperationAndIncreaseClock replicaId state op = 
  { data: applyOperation state op, timestamp: VectorClock.increment state.timestamp replicaId }

applyOperations ::
  forall t op f. Effector t op => Foldable f => WithTimestamp t -> f (WithTimestamp op) -> t
applyOperations state ops = (foldl applyOperation' state ops).data
  where applyOperation' acc op = state { data = applyOperation acc op }

applyOperationsAndIncreaseClock ::
  forall t op f. Effector t op => Foldable f => ReplicaId -> WithTimestamp t -> f (WithTimestamp op) -> WithTimestamp t
applyOperationsAndIncreaseClock replicaId = foldl (applyOperationAndIncreaseClock replicaId)


-- | A default merge operation that uses the Semigroup instance for t
defaultMerge :: forall t. Semigroup t => (WithTimestamp t -> WithTimestamp t -> t)
defaultMerge a b = a.data <> b.data

mergeStates :: forall t. StateBasedCRDT t => WithTimestamp t -> WithTimestamp t -> WithTimestamp t
mergeStates a b = { data: merge a b, timestamp: a.timestamp <> b.timestamp }