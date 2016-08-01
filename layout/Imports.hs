{-# LANGUAGE DeriveFunctor #-}
module Imports (
  ImportError(..),
  getImportDefs,
  validateImports,
  fixAllPaths
  ) where

import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable ()
import Data.Monoid
import Data.Traversable ()
import System.FilePath

import DAG
import ParserTypes
import Qualify

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
  DuplicateImport {
    importErrMainFile :: FileName,
    importErrRelImport :: FileName
    }|
  DuplicateFile {
    importErrMainFile :: FileName
  }

convertDAGError :: DAGError FileName -> ImportError
convertDAGError (DAGMissing x k) = MissingImport x k
convertDAGError (DAGCycle x) = ImportCycle x

convertQualifyError :: FileName -> QualifyError FileName Identifier -> ImportError
convertQualifyError m (LocalNameMissing x k) = MissingSymbol m k x
convertQualifyError m (GlobalNameMissing x) = MissingImport m x
convertQualifyError m (LocalNameDuplicate x k) = DuplicateSymbol m x k
convertQualifyError m (GlobalNameDuplicate x) = DuplicateImport m x 

-- In both of these functions, all filenames must be collapsed and relative
-- to a common base

getImportDefs :: FileName ->
                 Map FileName (Map ContractName a)) ->
                 [(FileName, ImportAs)] ->
                 Either ImportError (Map ContractName a)
getImportDefs mainFileName fileDefs imports =
  importsM <- first (convertQualifyError mainFileName) $ getQualifiedNames imports' fileDefs
  Map.foldrWithKey unionWithError (return Map.empty) importsM
  where
    imports' = map (second convertImportAs) imports
    convertImportAs Unqualified = QualifyAll id
    convertImportAs (StarPrefix p) = QualifyAll ((p ++ ".") ++)
    convertImportAs (Aliases as) = QualifySome as
    unionWithError x m1 m2E = Map.unionWithKey (\k _ _ -> Left $ DuplicateSymbol mainFileName x k) m1 <$> m2E

validateImports :: Map FileName SolidityFile -> Either ImportError ()
validateImports files = 
  first convertDAGError $ checkDAG elem $ Map.map (map fst . fileImports) files

fixAllPaths :: Map FileName SolidityFile -> Either ImportError (Map FileName SolidityFile)
fixAllPaths filesM =
  fmap makeImportsRelative $ mapKeysWithKey handleCollision $ map Right $ collapse filesM
  where handleCollision fn f1 f2
          | f1 == f2 = f2
          | otherwise = Left $ DuplicateFile fn

makeImportsRelative :: Map FileName SolidityFile -> Map FileName SolidityFile
makeImportsRelative = Map.mapWithKey $ modifyImportsBy . takeDirectory
  where modifyImportsBy fn f = f{fileImports = map (first $ fn <//>) $ fileImports f}

(<//>) :: FilePath -> FilePath -> FilePath
mp <//> fn = collapse $ prependIfRelative mp fn

prependIfRelative :: FilePath -> FilePath -> FilePath
prependIfRelative mp fn =
  case splitDirectories fn of
    "." : _ -> mp </> fn
    ".." : _ -> mp </> fn
    _ -> fn

collapse :: FilePath -> FilePath
collapse path = joinPath $ collapse' $ splitDirectories path
  where collapse' [] = []
        collapse' (_ : ".." : rest) = collapse' rest
        collapse' ("." : x : rest) = collapse' $ x : rest
        collapse' (x : rest) = x : collapse' rest

