module Inheritance (doInheritance) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Map as Map

import ParserTypes
import C3

doInheritance :: ContractsByID -> ContractsByID
doInheritance contracts =
  makeInheritancePaths contracts $ Map.map (doContractInheritance contracts) files

doContractInheritance :: ContractsByID -> SolidityContract -> SolidityContract
doContractInheritance contracts contract =
  Map.map combineContracts $ c3Linearize inheritMap
  where
    inheritMap = Map.map (getInheritsList files) contracts
    combineContracts cIDs = foldr1 combine . NonEmpty.map (contracts Map.!)
  
getInheritsList :: ContractsByID -> SolidityContract -> [ContractID]
getInheritsList contracts c{contractInheritancePaths = UnresolvedBases l} = map lookupName l
  where
    lookupName cN = Map.findWithDefault (theError cN) (ContractID f cN) contracts
    theError cN = error $ "Invalid base name " ++ show cN ++ " in contract " ++ show n
    ContractID{contractRealFile = f, contractRealName = n} = contractID c 

getInheritsList _ = error $ "Inheritance paths constructor should be UnresolvedBases"

combine :: SolidityContract -> SolidityContract -> SolidityContract
combine c1 c2 = 
  Contract {
    contractID = contractID c1,
    contractOwnDeclarations = contractOwnDeclarations c1,
    contractAllDeclarations = contractOwnDeclarations c1 : contractAllDeclarations c2,
    contractInheritancePaths = contractInheritancePaths c1, -- Computed separately
    contractIsLibrary = contractIsLibrary c1
  }

makeInheritancePaths :: ContractsByID -> ContractsByID
makeInheritancePaths contracts = result
  where result = Map.map (makeContractInheritancePaths result) contracts

makeContractInheritancePaths :: ContractsByID -> SolidityContract -> SolidityContract
makeContractInheritancePaths contracts c{contractID = cID} =
  c{contractInheritancePaths = makeInheritanceMap contracts cID $ contractInheritancePaths c}

makeInheritanceMap :: ContractsByID -> ContractID -> InheritanceMap -> InheritanceMap
makeInheritanceMap contracts cID (UnresolvedBases bs) = 
  InheritanceMap{inheritanceLeaf = cID, inheritanceBases = map getInheritanceMap bs}
  where
    getInheritanceMap b = (theCID b, contractInheritancePaths $ lookupName b contracts)
    lookupName b = Map.findWithDefault (theError b) $ theCID b
    theError cN = error $ "Invalid base name " ++ show cN ++ " in contract " ++ show n 
    theCID b = ContractID f b
    ContractID{contractRealFile = f, contractRealName = n} = cID 

makeInheritanceMap _ _ _ = error $ "Inheritance paths constructor should be UnresolvedBases"

