module Structure (test) where

import Test.Common

import qualified Structure.BasicTypes as BasicTypes
import qualified Structure.CompositeTypes as CompositeTypes
import qualified Structure.Events as Events
-- import qualified Structure.DeclarationModifiers as DeclarationModifiers
-- import qualified Structure.ErrorMessages as ErrorMessages
import qualified Structure.Functions as Functions
import qualified Structure.Imports as Imports
import qualified Structure.Inheritance as Inheritance
import qualified Structure.Layout as Layout
import qualified Structure.Linkage as Linkage

test :: TestTree
test = testGroup "structure" [
  BasicTypes.test,
  CompositeTypes.test,
  Events.test,
  Functions.test,
--  DeclarationModifiers.test,
--  ErrorMessages.test,
  Imports.test,
  Inheritance.test,
  Layout.test,
  Linkage.test
  ]
