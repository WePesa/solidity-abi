module Structure.Linkage.Names (test) where

import qualified Data.Map as Map

import Blockchain.Ethereum.Solidity
import Structure.Common
import Test.Combinators
import Test.Common

test :: TestTree
test = doTests "names" structureTest [
  newStructName,
  newEnumName,
  inheritedTypeName,
  qualifiedInheritedTypeName,
  libraryTypeName,
  contractTypeName
  ]

newStructName :: StructureTestInput
newStructName = (name, sources, tester)
  where
    name = "newStructName"
    sources = Map.singleton name $
      contractDefn "C" $
        structDefn "S" ["int"] ##
        varDecl "S" "x"
    tester contracts = contractHasPlainLink name contracts "C" $ WithSize 32 dID
    dID = DeclID cID "S"
    cID = ContractID name "C"

newEnumName :: StructureTestInput
newEnumName = (name, sources, tester)
  where
    name = "newEnumName"
    sources = Map.singleton name $
      contractDefn "C" $
        enumDefn "E" ["A"] ##
        varDecl "E" "x"
    tester contracts = contractHasPlainLink name contracts "C" $ WithSize 1 dID
    dID = DeclID cID "E"
    cID = ContractID name "C"

inheritedTypeName :: StructureTestInput
inheritedTypeName = (name, sources, tester)
  where
    name = "inheritedTypeName"
    sources = Map.singleton name $
      contractDefnBases "C" (varDecl "S" "x") ["D"] ##
      contractDefn "D" (structDefn "S" ["int"])
    tester contracts = 
      contractHasPlainLink name contracts "C" $ WithSize 32 dID
    dID = DeclID cID "S"
    cID = ContractID name "D"

qualifiedInheritedTypeName :: StructureTestInput
qualifiedInheritedTypeName = (name, sources, tester)
  where
    name = "qualifiedInheritedTypeName"
    sources = Map.singleton name $
      contractDefnBases "C" (varDecl "D.S" "x") ["D"] ##
      contractDefn "D" (structDefn "S" ["int"])
    tester contracts = 
      contractHasInheritedLink name contracts "C" (ContractID name "D") "S" $ WithSize 32

libraryTypeName :: StructureTestInput
libraryTypeName = (name, sources, tester)
  where
    name = "libraryTypeName"
    sources = Map.singleton name $
      libraryDefn "L" (structDefn "S" ["int"]) ##
      contractDefn "C" (varDecl "L.S" "x")
    tester contracts =
      contractHasLibraryLink name contracts "C" (ContractID name "L") "S" $ WithSize 32

contractTypeName :: StructureTestInput
contractTypeName = (name, sources, tester)
  where
    name = "contractTypeName"
    sources = Map.singleton name $
      contractDefn "C" (varDecl "D" "x") ##
      contractDefn "D" ""
    tester contracts = contractHasContractLink name contracts "C" "D"
