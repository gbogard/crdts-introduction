module Test.Data.CRDT.Laws where

import Data.CRDT
import Prelude

import Data.Replication (ReplicaId)
import Test.QuickCheck (class Arbitrary, Result, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy)

stateBasedCRDTLaws :: 
  forall t. StateBasedCRDT t => Arbitrary t => Eq t => Show t => String -> Proxy t ->  Spec Unit
stateBasedCRDTLaws name _ =
  describe (name <> " laws") do
    it "should be associative" $
      quickCheck associativity
    it "should be commutative" do
      quickCheck commutativity
    it "should be idempotent" do
      quickCheck idempotence
  where
    associativity :: WithTimestamp t -> WithTimestamp t -> WithTimestamp t -> Result
    associativity a b c = (mergeStates (mergeStates a b) c) === (mergeStates a (mergeStates b c))
    commutativity :: WithTimestamp t -> WithTimestamp t -> Result
    commutativity a b = (mergeStates a b) === (mergeStates b a)
    idempotence :: WithTimestamp t -> WithTimestamp t -> Result
    idempotence a b = (mergeStates a b) === (mergeStates a >>> mergeStates a >>> mergeStates a $ b)

operationBasedCRDTLaws ::
  forall t op. Generator t op 
    => Effector t op 
    => Arbitrary t 
    => Arbitrary op 
    => Show op 
    => Eq op
    => Show t 
    => Eq t
    => String -> Proxy t -> Spec Unit
operationBasedCRDTLaws name _ =
  describe (name <> " laws") do
    it "should be commutative" $
      quickCheck commutativity
    it "should have complementary generateOperations / applyOperation functions" $
      quickCheck applyGenerateEquivalence
  where
    commutativity :: ReplicaId -> WithTimestamp op -> WithTimestamp op -> WithTimestamp t -> Result
    commutativity replica a b state =
      applyOperationAndIncreaseClock replica (applyOperationAndIncreaseClock replica state b) a ===
      applyOperationAndIncreaseClock replica (applyOperationAndIncreaseClock replica state a) b
    applyGenerateEquivalence :: ReplicaId -> WithTimestamp t -> WithTimestamp t -> Result
    applyGenerateEquivalence replica a b = 
      applyOperations a opsWithTimestamp === b.data
      where 
        opsWithTimestamp = {data: _, timestamp: a.timestamp} <$> generateOperations a.data b.data
    