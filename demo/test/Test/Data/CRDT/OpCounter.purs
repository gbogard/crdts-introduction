module Test.Data.CRDT.OpCounter where

import Prelude
import Test.Spec

import Data.CRDT.OpCounter (OpCounter)
import Test.Data.CRDT.Laws (operationBasedCRDTLaws)
import Type.Proxy (Proxy(..))

opCounterSpec :: Spec Unit
opCounterSpec =
  describe "OPCounter - Operation-based counter" do
    operationBasedCRDTLaws "" (Proxy :: Proxy OpCounter)
