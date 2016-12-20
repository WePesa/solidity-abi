module Structure.Layout (test) where

import qualified Data.Map as Map

import Blockchain.Ethereum.Solidity
import Structure.Common
import Test.Combinators
import Test.Common

import qualified Structure.Layout.Sizes as Sizes
import qualified Structure.Layout.Storage as Storage

test :: TestTree
test = testGroup "layout" [
  Sizes.test,
  Storage.test
  ]
