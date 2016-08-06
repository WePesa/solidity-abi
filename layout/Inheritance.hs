module Inheritance (doInheritance) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Map as Map

import ParserTypes
import C3

doInheritance :: SolidityContracts -> SolidityContracts
doInheritance contracts = Map.map combineContracts $ c3Linearize inheritMap
  where
    inheritMap = Map.map (map fst . contractInherits) contracts
    combineContracts = foldr1 combine . NonEmpty.map (contracts Map.!)
  
combine :: SolidityContract -> SolidityContract -> SolidityContract
combine c1 c2 = 
  -- TODO: qualify the names of imported vars, funcs, events, modifiers
  Contract {
    contractName = contractName c1,
    -- objects are prepended to the list during parsing: later ones first
    contractVars = contractVars c1 ++ contractVars c2,
    contractFuncs = contractFuncs c1 `Map.union` contractFuncs c2,
    contractEvents = contractEvents c1 `Map.union` contractEvents c2,
    contractModifiers = contractModifiers c1 `Map.union` contractModifiers c2,
    -- derived contract types override base ones.  The originals are
    -- available in contractLibraryTypes.
    -- TODO: inherited types have their layout done twice like this
    contractTypes = contractTypes c1 `Map.union` contractTypes c2,
    contractLibraryTypes =
      let
        libUnion = Map.unionWith Set.union
        typesLib = Map.singleton (contractName c2) $
                   Map.foldr (Set.insert . typeName) Set.empty $ contractTypes c2
      in typesLib `libUnion` contractLibraryTypes c1 `libUnion` contractLibraryTypes c2,
    -- order of inheritance is right-to-left
    contractInherits = contractInherits c1 ++ contractInherits c2, 
    contractIsLibrary = contractIsLibrary c1
  }

