module Structure.Linkage.Libraries (test) where

import qualified Data.Map as Map

import Blockchain.Ethereum.Solidity
import Structure.Common
import Test.Combinators
import Test.Common

test :: TestTree
test = doTests "libraries" structureTest [
  libraryDefinition,
  libraryTypeBasicVar,
  libraryTypeArray,
  libraryTypeMapping,
  libraryTypeFunction,
  libraryTypeStruct,
  multipleLibrariesReferenced
  ]

libraryDefinition :: StructureTestInput
libraryDefinition = (name, sources, tester)
  where
    name = "libraryDefinition"
    sources = Map.singleton name $
      libraryDefn "L" ""
    tester contracts = fileContractIsLibrary name contracts "L"

libraryTypeBasicVar :: StructureTestInput
libraryTypeBasicVar = libraryTypeTest "libraryTypeBasicVar" varDecl

libraryTypeArray :: StructureTestInput
libraryTypeArray = 
  libraryTypeTest "libraryTypeArray" $ \t n -> varDecl (arrayDeclType t "3") n

libraryTypeMapping :: StructureTestInput
libraryTypeMapping = 
  libraryTypeTest "libraryTypeMapping" $ \t n -> varDecl (mappingDeclType "int" t) n

libraryTypeFunction :: StructureTestInput
libraryTypeFunction = 
  libraryTypeTest "libraryTypeFunction" $ \t n -> functionDecl n [t] []

libraryTypeStruct :: StructureTestInput
libraryTypeStruct =
  libraryTypeTest "libraryTypeStruct" $ \t n -> structDefn n [t]

multipleLibrariesReferenced :: StructureTestInput
multipleLibrariesReferenced = (name, sources, tester)
  where
    name = "multipleLibrariesReferenced"
    sources = Map.singleton name $
      libraryDefn "L1" (structDefn "S1" ["int"]) ##
      libraryDefn "L2" (structDefn "S2" ["int"]) ##
      contractDefn "C" (
        varDecl "L1.S1" "x1" ##
        varDecl "L2.S2" "x2"
        )
    tester contracts = 
      contractHasAllLibraries name contracts "C" $ map (ContractID name) ["L1", "L2"]

libraryTypeTest :: String -> (String -> String -> String) -> StructureTestInput
libraryTypeTest name declMaker = (name, sources, tester)
  where
    sources = Map.singleton name $
      libraryDefn "L" (structDefn "S" ["int"]) ##
      contractDefn "C" (declMaker "L.S" "x")
    tester contracts = contractHasLibrary name contracts "C" $ ContractID name "L"

