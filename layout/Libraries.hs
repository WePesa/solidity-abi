module Libraries (
  validateLibraries,
  getLibraryLayouts,
  LibraryError(..)
  ) where

import DAG
import Qualify
import DefnTypes
import LayoutTypes
import ParserTypes

import Data.Bifunctor
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

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
    }

convertDAGError :: DAGError ContractName -> LibraryError
convertDAGError (DAGMissing c l) = LibraryMissing c l
convertDAGError (DAGCycle c) = LibraryCycle c

convertQualifyError :: ContractName -> QualifyError ContractName Identifier Identifier -> LibraryError
convertQualifyError c (LocalNameMissing l t) = LibraryTypeMissing c l t
convertQualifyError c (GlobalNameMissing l) = LibraryMissing c l
convertQualifyError c (LocalNameDuplicate l t) = LibraryTypeDuplicate c l t

getLibraryLayouts :: ContractName ->
                     SolidityContractsLayout -> 
                     [(ContractName, [Identifier])] -> 
                     Either LibraryError (Map ContractName SolidityTypesLayout)
getLibraryLayouts name contractsL libImports = 
  first (convertQualifyError name) $ getQualifiedNames qAs allLibs 
  where
    qAs = map (second $ QualifySome . map (\a -> (a, a))) $ nub libImports
    allLibs = Map.map typesLayout contractsL

validateLibraries :: SolidityContractsDef -> Either LibraryError ()
validateLibraries contractDefs = 
  first convertDAGError $ checkDAG $ Map.map (map fst . libraryTypes) contractDefs

