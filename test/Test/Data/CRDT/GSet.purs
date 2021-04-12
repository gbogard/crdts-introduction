module Test.Data.CRDT.GSet where


import Prelude
import Test.Spec (Spec, describe, it)
import Data.CRDT.GSet (GSet, insert, query)
import Data.HashSet as Set
import Test.Data.CRDT.Laws (stateBasedCRDTLaws)
import Test.QuickCheck (Result, assertEquals, (===))
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

gSetSpec :: Spec Unit
gSetSpec = 
  describe "GSet - Grow-only set" do
    stateBasedCRDTLaws "Int" (Proxy :: Proxy (GSet Int))
    stateBasedCRDTLaws "String" (Proxy :: Proxy (GSet String))
    it "can always retrieve the element after insertion" $
      quickCheck retrievalAfterInsertion
    it "has idempotent insert" $
      quickCheck idempotentInsert 
  where
    retrievalAfterInsertion :: Int -> GSet Int -> Result
    retrievalAfterInsertion el = insert el >>> query >>> Set.member el >>> assertEquals true
    
    idempotentInsert :: Int -> GSet Int -> Result
    idempotentInsert el gset = insert el gset === (insert el >>> insert el $ gset)
    