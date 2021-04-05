module Test.Data.CRDT.GCounter where

import Prelude

import Data.CRDT.GCounter (GCounter, increment, query)
import Data.Replication (ReplicaId)
import Test.Data.CRDT.Laws (stateBasedCRDTLaws)
import Test.QuickCheck (Result, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

gCounterSpec :: Spec Unit
gCounterSpec =
  describe "GCounter - Grow-only counter" do
    stateBasedCRDTLaws "" (Proxy :: Proxy GCounter)
    it "should increment the current value" $
      quickCheck incrementCurrentValue
  where
    incrementCurrentValue :: GCounter -> ReplicaId -> Result
    incrementCurrentValue counter replica =
      (increment replica counter # query) === (query counter + 1)