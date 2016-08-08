module Inheritance (doInheritance) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Map as Map

import ParserTypes
import C3

doInheritance :: Map FileName SolidityContracts -> SolidityContract -> SolidityContract
doInheritance files contract = Map.map combineContracts $ c3Linearize inheritMap
  where
    inheritMap = Map.map (getInheritsList files) contractIDMap
    contractIDMap = Map.foldr Map.union Map.empty $ makeContractIDMap files
    combineContracts cIDs = foldr1 combine . NonEmpty.map (contractIDMap Map.!)
  
makeContractIDMap :: SolidityContracts -> Map ContractID SolidityContract
makeContractIDMap contracts = Map.foldr insertContractID Map.empty contracts
  where insertContractID c = Map.insert (contractID c)

getInheritsList :: Map FileName SolidityContracts -> SolidityContract -> [ContractID]
getInheritsList files c{contractInheritancePaths = UnresolvedBases l} =
  map (lookupName . fst) l
  where
    lookupName cN = Map.findWithDefault (theError cN) cN contracts
    theError cN = error $ "Invalid base name " ++ show cN ++ " in contract " ++ show n
    contracts = files Map.! f
    ContractID{contractRealFile = f, contractRealName = n} = cID c 

getInheritsList _ = error $ "Inheritance paths constructor should be UnresolvedBases"

combine :: SolidityContract -> SolidityContract -> SolidityContract
combine c1 c2 = 
  Contract {
    contractID = contractID c1,
    contractOwnDeclarations = contractOwnDeclarations c1,
    contractAllDeclarations = contractOwnDeclarations c1 : contractAllDeclarations c2,
    contractInheritancePaths = contractInheritancePaths c1, -- Fix this
    contractIsLibrary = contractIsLibrary c1
  }

