module Test.Data.CRDT.OpCounter where

import Prelude

import Data.CRDT.OpCounter (OpCounter)
import Test.Data.CRDT.Laws (operationBasedCRDTLaws)
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy(..))

opCounterSpec :: Spec Unit
opCounterSpec =
  describe "OPCounter - Operation-based counter" do
    operationBasedCRDTLaws "" (Proxy :: Proxy OpCounter)
