module Data.Replication where

import Prelude

import Data.Hashable (class Hashable)
import Test.QuickCheck (class Arbitrary)

newtype ReplicaId = ReplicaId Int

derive newtype instance showReplicaId :: Show ReplicaId
derive newtype instance eqReplicaId :: Eq ReplicaId
derive newtype instance ordReplicaId :: Ord ReplicaId
derive newtype instance hashableReplicaId :: Hashable ReplicaId
derive newtype instance arbitraryReplicaId :: Arbitrary ReplicaId