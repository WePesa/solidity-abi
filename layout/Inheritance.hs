module Inheritance (doInheritance) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Map as Map

import SolidityTypes
import C3

doInheritance :: ContractsByName -> ContractsByName
doInheritance contracts = 
  Map.map combineContracts $ c3Linearize $ Map.map contractInherits contracts
  where combineContracts = foldr1 combine . NonEmpty.map (contracts Map.!)

combine :: SolidityContract -> SolidityContract -> SolidityContract
combine c1 c2 = 
  Contract {
    contractVars = (combineDeclsBy `on` contractVars) c1 c2,
    contractFuncs = (combineDeclsBy `on` contractFuncs) c1 c2,
    contractEvents = (combineDeclsBy `on` contractEvents) c1 c2,
    contractModifiers = (combineDeclsBy `on` contractModifiers) c1 c2,
    contractTypes = (combineDeclsBy `on` contractTypes) c1 c2,
    -- Derived contracts have later storage locations, i.e. come first
    contractStorageVars = contractStorageVars c2 ++ contractStorageVars c1,
    contractInherits = contractInherits c1,
    contractExternalNames = (Set.union `on` contractExternalNames) c1 c2,
    contractLibraryTypes = (Set.union `on` contractLibraryTypes) c1 c2,
    contractIsConcrete = all funcHasCode $ declaredFuncs $ contractDeclarationsByName c2,
    contractIsLibrary = contractIsLibrary c1
  }

combineDeclsBy :: DeclarationsBy a -> DeclarationsBy a -> DeclarationsBy a
combineDeclsBy d1 d2 = 
  DeclarationsBy {
    -- Declarations in the derived contract override those with the same
    -- name in the base contract
    byName = (Map.union `on` byName) d1 d2,
    byID = (Map.union `on` byID) d1 d2
    }

