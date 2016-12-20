module Structure.Linkage (test) where

import Test.Common

import qualified Structure.Linkage.Libraries as Libraries
import qualified Structure.Linkage.Names as Names

test :: TestTree
test = testGroup "linkage" [
  Libraries.test,
  Names.test 
  ]
