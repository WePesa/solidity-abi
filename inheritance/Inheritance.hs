-- |
-- Module: Inheritance
-- Description: Resolves all import _and_ inheritance in a set of contracts by file
-- Maintainer: Ryan Reich <ryan@blockapps.net>
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Inheritance (doInheritance) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC8

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)

import Data.Text (Text)

import SolidityTypes
import BaseContracts
import C3
import DAG
import FilePaths
import Qualify

-- | Resolves imports and inheritance in a set of contracts by file.  This
-- must be done interleaved because a contract may derive an aliased
-- imported contract name, which nonetheless should already have its
-- inheritance resolved.
doInheritance :: SolidityFiles -> ContractsByID 'AfterInheritance
doInheritance = mergeBaseContracts .  makeContractsByID .  importAndLinearize .  fixAllPaths 

makeContractsByID :: ContractsByFile 'AfterInheritance -> ContractsByID 'AfterInheritance
makeContractsByID = Map.foldrWithKey combine Map.empty
  where combine k = Map.union . Map.mapKeys (ContractID k)

importAndLinearize :: SolidityFiles -> ContractsByFile 'AfterInheritance
importAndLinearize files = either throwImportError id $ do
  validateImports files
  let resolved = Map.mapWithKey (linearizeFile resolved) files
  return resolved

linearizeFile :: ContractsByFile 'AfterInheritance -> FileName -> SolidityFile ->
                 ContractsByName 'AfterInheritance
linearizeFile resolved name SolidityFile{fileContracts, fileImports} = 
  either (error $ "Import error") id $ do
    imported <- getImportDefs resolved name fileImports
    return $ contractsL `Map.union` imported  

  where
    contractsL = Map.mapWithKey (linearizeContract basesL . ContractID name) fileContracts
    basesL = Map.map (allBases . contractBases) $ makeContractsByID resolved

linearizeContract :: Map ContractID (NonEmpty ContractID) ->
                     ContractID ->
                     Contract 'AfterParsing ->
                     Contract 'AfterInheritance
linearizeContract 
  basesL
  cID
  contract@Contract{
    contractBases, contractTypes, contractStorageVars, contractLinkage
    } =
  contract{contractBases = 
    BaseContracts {
      directBases = contractBases,
      allBases = c3Memoize basesL cID contractBases
      },
    contractTypes, contractStorageVars, contractLinkage
   }

getImportDefs :: ContractsByFile 'AfterInheritance -> FileName -> [(FileName, ImportAs)] ->
                 Either ImportError (ContractsByName 'AfterInheritance)
getImportDefs fileDefs mainFileName imports = do
  imported <- first (convertQualifyError mainFileName) $ getQualifiedNames imports' fileDefs
  return $ Map.foldr Map.union Map.empty imported
  where
    imports' = map (second convertImportAs) imports
    convertImportAs Unqualified = QualifyAll id
    convertImportAs (StarPrefix p) = QualifyAll ((p ++ ".") ++)
    convertImportAs (Aliases as) = QualifySome as
    unionWithError x = 
      Map.unionWithKey (\k _ _ -> Left $ DuplicateSymbol mainFileName x k) . Map.map Right

validateImports :: Map FileName SolidityFile -> Either ImportError ()
validateImports files = 
  first convertDAGError $ checkDAG $ Map.map (map fst . fileImports) files

data ImportError = 
  ImportCycle {
    importErrMainFile :: FileName
    } |
  MissingImport {
    importErrMainFile :: FileName,
    importErrRelImport :: FileName
    } |
  MissingSymbol {
    importErrMainFile :: FileName,
    importErrSymbol :: Identifier,
    importErrRelImport :: FileName
    } |
  DuplicateSymbol {
    importErrMainFile :: FileName,
    importErrRelImport :: FileName,
    importErrSymbol :: Identifier
    } |
  DuplicateFile {
    importErrMainFile :: FileName
  }

convertDAGError :: DAGError FileName -> ImportError
convertDAGError (DAGMissing x k) = MissingImport x k
convertDAGError (DAGCycle x) = ImportCycle x

convertQualifyError :: FileName -> QualifyError FileName ContractName ContractName -> ImportError
convertQualifyError m (LocalNameMissing x k) = MissingSymbol m k x
convertQualifyError m (GlobalNameMissing x) = MissingImport m x
convertQualifyError m (LocalNameDuplicate x k) = DuplicateSymbol m x k



throwImportError :: ImportError -> a
throwImportError (MissingImport fBase fName) =
  error $ BSC8.unpack $ encode $ object [
    "error" .= ("importError" :: Text),
    "importError" .= ("missingImport" :: Text),
    "missingImport" .= fName,
    "inFile" .= fBase
    ]

throwImportError _ = error $ "An import error occurred, probably a cycle"

