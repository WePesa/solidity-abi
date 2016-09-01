module BaseContracts (mergeBaseContracts) where

import Data.Function

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import SolidityTypes

mergeBaseContracts :: ContractsByID 'AfterInheritance -> ContractsByID 'AfterInheritance
mergeBaseContracts contracts = Map.map (combineContracts . allBases . contractBases) contracts
  where combineContracts = foldr1 combine . NonEmpty.map (contracts Map.!)

combine :: Contract 'AfterInheritance -> Contract 'AfterInheritance -> 
           Contract 'AfterInheritance
combine c1 c2 = -- c1 is derived from c2
  Contract {
    contractVars = (combineDeclsBy `on` contractVars) c1 c2,
    contractFuncs = newFuncDecls,
    contractEvents = (combineDeclsBy `on` contractEvents) c1 c2,
    contractModifiers = (combineDeclsBy `on` contractModifiers) c1 c2,
    contractTypes = (combineDeclsBy `on` contractTypes) c1 c2,
    -- Derived contracts have later storage locations, i.e. come first
    contractStorageVars = contractStorageVars c1 ++ contractStorageVars c2,
    contractBases = contractBases c1,
    contractLinkage = (Map.union `on` contractLinkage) c1 c2,
    contractIsConcrete = 
      all funcHasCode $ Map.map (byID newFuncDecls Map.!) $ byName newFuncDecls,
    contractIsLibrary = contractIsLibrary c1
  }

  where newFuncDecls = (combineDeclsBy `on` contractFuncs) c1 c2

combineDeclsBy :: DeclarationsBy a -> DeclarationsBy a -> DeclarationsBy a
combineDeclsBy d1 d2 = 
  DeclarationsBy {
    -- Declarations in the derived contract override those with the same
    -- name in the base contract
    byName = (Map.union `on` byName) d1 d2,
    byID = (Map.union `on` byID) d1 d2
    }

