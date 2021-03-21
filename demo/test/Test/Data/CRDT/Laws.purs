module Test.Data.CRDT.Laws where

import Prelude

import Data.CRDT (class StateBasedCRDT, State, mergeStates)
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
    associativity :: State t -> State t -> State t -> Result
    associativity a b c = (mergeStates (mergeStates a b) c) === (mergeStates a (mergeStates b c))
    commutativity :: State t -> State t -> Result
    commutativity a b = (mergeStates a b) === (mergeStates b a)
    idempotence :: State t -> State t -> Result
    idempotence a b = (mergeStates a b) === (mergeStates a >>> mergeStates a >>> mergeStates a $ b)