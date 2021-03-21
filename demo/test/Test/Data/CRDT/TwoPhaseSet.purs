module Test.Data.CRDT.TwoPhaseSet where

import Prelude

import CRDT.TwoPhaseSet (TwoPhaseSet, insert, query, remove)
import Data.HashSet as Set
import Test.Data.CRDT.Laws (stateBasedCRDTLaws)
import Test.QuickCheck (Result, assertEquals)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

twoPhaseSetSpec :: Spec Unit
twoPhaseSetSpec =
  describe "2PSet - Two-phase Set" do
    stateBasedCRDTLaws "Int" (Proxy :: Proxy (TwoPhaseSet Int))
    stateBasedCRDTLaws "String" (Proxy :: Proxy (TwoPhaseSet String))
    it "should be able to retrieve an element after insertion" $
      quickCheck retrievalAfterInsertion
    it "should not be able to retrieve an element after removal" $
      quickCheck retrievalAfterRemoval
  where
    retrievalAfterInsertion :: Int -> TwoPhaseSet Int -> Result
    retrievalAfterInsertion el = insert el >>> query >>> Set.member el >>> assertEquals true

    retrievalAfterRemoval :: Int -> TwoPhaseSet Int -> Result
    retrievalAfterRemoval el = 
      insert el >>> remove el >>> query >>> Set.member el >>> assertEquals false