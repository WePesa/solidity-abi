module Inheritance (doInheritance) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Map as Map

import SolidityTypes
import C3

doInheritance :: ContractsByName -> ContractsByName
doInheritance contracts = result
  where 
    result = Map.map combineContracts $ c3Linearize $ Map.map contractInherits contracts
    combineContracts = foldr (combine result) emptyContract . NonEmpty.map (contracts Map.!)

combine :: ContractsByName -> SolidityContract -> SolidityContract -> SolidityContract
combine contracts c1 c2 = 
  Contract {
    contractVars = (combineDeclsBy `on` contractVars) c1 c2,
    contractFuncs = (combineDeclsBy `on` contractFuncs) c1 c2,
    contractEvents = (combineDeclsBy `on` contractEvents) c1 c2,
    contractModifiers = (combineDeclsBy `on` contractModifiers) c1 c2,
    contractTypes = (combineDeclsBy `on` contractTypes) c1 c2,
    -- Derived contracts have later storage locations, i.e. come first
    contractStorageVars = contractStorageVars c2 ++ contractStorageVars c1,
    contractInherits = contractInherits c1,
    contractExternalNames = checkExternalNames c1 `Set.union` contractExternalNames c2,
    contractLibraryTypes = checkLibraryTypes c1 `Set.union` contractLibraryTypes c2,
    contractIsConcrete = all funcHasCode $ declaredFuncs $ contractDeclarationsByName c2,
    contractIsLibrary = contractIsLibrary c1
  }

  where
    checkExternalNames = 
      Set.map (checkExternalName contracts c1) . contractExternalNames
    newLibraryTypes = 
      Set.map makeID . Set.filter (isLibraryType contracts) . contractExternalNames 

combineDeclsBy :: DeclarationsBy a -> DeclarationsBy a -> DeclarationsBy a
combineDeclsBy d1 d2 = 
  DeclarationsBy {
    -- Declarations in the derived contract override those with the same
    -- name in the base contract
    byName = (Map.union `on` byName) d1 d2,
    byID = (Map.union `on` byID) d1 d2
    }

checkExternalName :: ContractsByName -> SolidityContract ->
                     ([ContractName], Identifier) -> ([ContractName], Identifier)
checkExternalName _ _ ([], name) =
  error $ "Empty contract qualifier in external name " ++ name
checkExternalName contracts c x@([cName], name) =
  | cName `elem` contractInherits c &&
    name `Map.member` (byName $ contractTypes $ contracts Map.! cName) 
    = x
  | otherwise
    = error $ "Type " ++ name ++ " not visible in contract " ++ cName
checkExternalName contracts _ x@(cName:rest, name) =
  checkExternalName contracts (contracts Map.! cName) (rest,name) `seq` x
 
checkLibraryType :: ContractsByName -> 
                    ([ContractName], Identifier) -> ([ContractName], Identifier)
checkLibraryType _ ([], name) = 
  error $ "Empty contract qualifier when checking library type " ++ name
checkLibraryType contracts x@([cName], name) =
  Map.findWithDefault False cName $ Map.map contractIsLibrary contracts 

makeID :: ([ContractName], Identifier) -> DeclID
makeID ([cName], name) = DeclID{ declContract = cName, declName = name}
makeID _ = error $ "Too many qualifiers in library name"
