module Json (test) where

import Distribution.TestSuite
 
import qualified Json.Imports as Imports

test :: Test
test = Group "json" False [
  Imports.test
  ]
