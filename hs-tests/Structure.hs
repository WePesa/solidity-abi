module Structure (test) where

import Test.Common

import qualified Structure.BasicTypes as BasicTypes
import qualified Structure.CompositeTypes as CompositeTypes
import qualified Structure.Functions as Functions
-- import qualified Structure.DeclarationModifiers as DeclarationModifiers
import qualified Structure.Imports as Imports
-- import qualified Structure.Inheritance as Inheritance
-- import qualified Structure.Contracts as Contracts
-- import qualified Structure.Libraries as Libraries

test :: TestTree
test = testGroup "structure" [
  BasicTypes.test,
  CompositeTypes.test,
  Functions.test,
--  DeclarationModifiers.test,
  Imports.test
--  BaseContracts.test,
--  Contracts.test,
--  Libraries.test
  ]
