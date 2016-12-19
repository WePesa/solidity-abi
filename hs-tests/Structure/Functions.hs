module Structure.Functions (test, functionTestInput) where

import qualified Data.Map as Map

import Blockchain.Ethereum.Solidity
import Structure.Common
import Test.Combinators
import Test.Common

test :: TestTree
test = doTests "functions" structureTest [
  functionNoArgsNoVals,
  functionOneArg,
  functionOneVal,
  functionTwoArgs,
  functionTwoVals,
  functionUnnamedArg,
  functionUnnamedVal
  ]

functionNoArgsNoVals :: StructureTestInput
functionNoArgsNoVals = functionTestInput "functionNoArgsNoVals" "f" [] [] [] [] [] []

functionOneArg :: StructureTestInput
functionOneArg = 
  functionTestInput "functionOneArg" "f" 
    ["int"] ["x"] [SignedInt 32]
    [] [] []

functionOneVal :: StructureTestInput
functionOneVal = 
  functionTestInput "functionOneVal" "f" 
    [] [] [] 
    ["int"] ["x"] [SignedInt 32]

functionTwoArgs :: StructureTestInput
functionTwoArgs =
  functionTestInput "functionTwoArgs" "f"
    ["int", "uint[]"] ["x", "y"] [SignedInt 32, DynamicArray $ UnsignedInt 32]
    [] [] []

functionTwoVals :: StructureTestInput
functionTwoVals =
  functionTestInput "functionTwoVals" "f"
    [] [] []
    ["int", "uint[]"] ["x", "y"] [SignedInt 32, DynamicArray $ UnsignedInt 32]

functionUnnamedArg :: StructureTestInput
functionUnnamedArg =
  functionTestInput "functionUnnamedArg" "f" 
    ["int"] [""] [SignedInt 32]
    [] [] []

functionUnnamedVal :: StructureTestInput
functionUnnamedVal =
  functionTestInput "functionUnnamedVal" "f" 
    [] [] []
    ["int"] [""] [SignedInt 32]

functionTestInput :: String -> String -> [String] -> [Identifier] -> [BasicType] ->
                     [String] -> [Identifier] -> [BasicType] -> StructureTestInput
functionTestInput cName fName args argNames argTypes vals valNames valTypes =
  (cName, sources, tester)
  where
    sources = Map.singleton cName source
    source = 
      contractDefn cName $ functionDecl fName args' vals'
    tester contracts = 
      functionDefnIs cName contracts cName fName defn
    args' = zipWith (##) args argNames
    vals' = zipWith (##) vals valNames
    defn = FuncDef{
      funcVisibility = PublicVisible,
      funcHasCode = True,
      funcIsConstant = False,
      funcIsConstructor = False,
      funcArgType = TupleValue $ makeTuple argNames argTypes,
      funcValueType = TupleValue $ makeTuple valNames valTypes
      }
    makeTuple = zipWith $ \n t ->
      ArgDef{
        argName = n,
        argType = t,
        argStorage = MemoryStorage
        }

