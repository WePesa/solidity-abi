module Inheritance (doInheritance) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Map as Map

import SolidityTypes
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
getInheritsList contracts contract = map lookupName $ contractInherits contract
  where
    lookupName cN = Map.findWithDefault (theError cN) (ContractID f cN) contracts
    theError cN = error $ "Invalid base name " ++ show cN ++ " in contract " ++ show n
    ContractID{contractRealFile = f, contractRealName = n} = contractID contract

combine :: SolidityContract -> SolidityContract -> SolidityContract
combine c1 c2 = 
  Contract {
    contractID = contractID c1,
    -- Derived contracts have later storage locations, i.e. come first
    contractStorageVars = contractStorageVars c2 ++ contractStorageVars c1,
    contractDeclarationsByID = (combineDecls `on` contractDeclarationsByID) c1 c2,
    contractDeclarationsByName = (combineDecls `on` contractDeclarationsByName) c1 c2,
    contractLibraryTypes = (Map.unionWith Set.union `on` contractLibraryTypes) c1 c2,
    contractInherits = contractInherits c1,
    contractIsConcrete = 
      contractIsConcrete c1 && (
        contractIsConcrete c2 ||
        all funcHasCode $ declaredFuncs $ contractDeclarationsByName c2
        )
    contractIsLibrary = contractIsLibrary c1
  }

combineDecls :: ContractDeclarations a -> ContractDeclarations a -> ContractDeclarations a
combineDecls d1 d2 =
  Declarations {
    declaredvars = declaredVars d1 `Map.union` declaredVars d2,
    declaredFuncs = declaredFuncs d1 `Map.union` declaredFuncs d2,
    declaredEvents = declaredEvents d1 `Map.union` declaredEvents d2,
    declaredModifiers = declaredModifiers d1 `Map.union` declaredModifiers d2,
    declaredTypes = declaredTypes d1 `Map.union` declaredTypes d2
  }

