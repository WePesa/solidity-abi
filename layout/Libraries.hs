module Libraries (
  validateLibraries,
  getLibraryDefs,
  LibraryError(..)
  ) where

import DAG
import Qualify
import DefnTypes

import Data.Bifunctor
import Data.Function
import Data.List

data LibraryError =
  LibraryMissing {
    libraryErrorContract :: ContractName,
    libraryErrorLibrary :: ContractName 
    } |
  LibraryCycle {
    libraryErrorContract :: ContractName
    } |
  LibraryTypeMissing {
    libraryErrorContract :: ContractName,
    libraryErrorLibrary :: ContractName,
    libraryErrorType :: Identifier
    } |
  LibraryTypeDuplicate {
    libraryErrorContract :: ContractName,
    libraryErrorLibrary :: ContractName,
    libraryErrorType :: Identifier
    } |
  LibraryDuplicateImport {
    libraryErrorContract :: ContractName,
    libraryErrorLibrary :: ContractName
    }

convertDAGError :: DAGError ContractName -> LibraryError
convertDAGError (DAGMissing c l) = LibraryMissing c l
convertDAGError (DAGCycle c) = LibraryCycle c

convertQualifyError :: ContractName -> QualifyError ContractName Identifier -> LibraryError
convertQualifyError c (LocalNameMissing l t) = LibraryTypeMissing c l t
convertQualifyError c (GlobalNameMissing l) = LibraryMissing c l
convertQualifyError c (LocalNameDuplicate l t) = LibraryTypeDuplicate c l t
convertQualifyError c (GlobalNameDuplicate l) = LibraryDuplicateImport c l

getLibraryLayouts :: ContractName ->
                     SolidityContractsLayout -> 
                     [(ContractName, [Identifier])] -> 
                     Either LibraryError (Map ContractName SolidityTypesLayout)
getLibraryLayouts name contractsL libImports = 
  left (convertQualifyError name) $ getQualifiedNames qAs allLibs 
  where
    qAs = map (second $ QualifySome . map (\a -> (a, a))) libImports
    allLibs = Map.map typesLayout contractDefs

validateLibraries :: SolidityContractsDef -> Either LibraryError ()
validateLibraries contractDefs = 
  left convertDAGError $ checkDAG elem $ Map.map libraryTypesDef contractsDef

