import Test.Tasty

import qualified Structure
--import qualified Json

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "solidity-abi tests" [
  Structure.test
--  Json.test
  ]

