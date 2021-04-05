module Test.Data.CRDT.Laws where

import Prelude
import Test.QuickCheck (class Arbitrary, Result, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy)
import Data.CRDT
  ( class Effector
  , class Generator
  , class StateBasedCRDT
  , applyOperation
  , applyOperations
  , generateOperations
  , merge
  )

stateBasedCRDTLaws ::
  forall t. StateBasedCRDT t => Arbitrary t => Eq t => Show t => String -> Proxy t -> Spec Unit
stateBasedCRDTLaws name _ =
  describe (name <> " laws") do
    it "should be associative"
      $ quickCheck associativity
    it "should be commutative" do
      quickCheck commutativity
    it "should be idempotent" do
      quickCheck idempotence
    it "should have a neutral element" do
      quickCheck identity'
  where
  associativity :: t -> t -> t -> Result
  associativity a b c = (merge (merge a b) c) === (merge a (merge b c))

  commutativity :: t -> t -> Result
  commutativity a b = merge a b === merge b a

  idempotence :: t -> Result
  idempotence a = merge a a === a

  identity' :: t -> Result
  identity' a = (merge a mempty) === a

operationBasedCRDTLaws ::
  forall t op.
  Generator t op =>
  Effector t op =>
  Arbitrary t =>
  Arbitrary op =>
  Show op =>
  Eq op =>
  Show t =>
  Eq t =>
  String -> Proxy t -> Spec Unit
operationBasedCRDTLaws name _ =
  describe (name <> " laws") do
    it "should be commutative"
      $ quickCheck commutativity
    it "should have complementary generateOperations / applyOperation functions"
      $ quickCheck applyGenerateEquivalence
  where
  commutativity :: op -> op -> t -> Result
  commutativity a b state = applyOperation a (applyOperation b state) === applyOperation b (applyOperation a state)

  applyGenerateEquivalence :: t -> t -> Result
  applyGenerateEquivalence a b = applyOperations (generateOperations a b) a === b
