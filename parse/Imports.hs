module Imports where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable
import System.FilePath

import ParserTypes

data ImportError = 
  ImportCycle {
    importErrMainFile :: FileName,
    importErrRelImport :: FileName
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
  MissingBase {
    importErrMainFile :: FileName,
    importErrBase :: Identifier
    }

getImportDefs :: FileName ->
                 Map FileName (Either ImportError (Map ContractName a)) ->
                 [(FileName, ImportAs)] ->
                 Either ImportError (Map ContractName a)
getImportDefs mainFileName fileDefsEither imports = do
  imported <- mapM getQualifiedImports imports
  return $ Map.unions imported

  where
    getQualifiedImports (fileName, importAs) = do
      fileDef <- getFileEither fileDefsEither
      let symbolDefsEither = Map.map Right fileDef
      changeNames symbolDefsEither

      where
        getFileEither =
          Map.findWithDefault (Left $ MissingImport mainFileName relImport) relImport
        changeNames = case importAs of
          Unqualified -> sequence
          StarPrefix p -> sequence . Map.mapKeys ((p ++ ".") ++)
          Aliases as -> sequence . Map.fromList . flip map as . getSym
            where getSym m (k, x) = (x, getSymbolEither k m)
        getSymbolEither sym =
          Map.findWithDefault (Left $ MissingSymbol mainFileName sym relImport) sym
        relImport = collapse $ prependIfRelative fileName

    prependIfRelative fn =
      case splitDirectories fn of
        "." : _ -> mainFilePath </> fn
        ".." : _ -> mainFilePath </> fn
        _ -> fn
      where mainFilePath = takeDirectory mainFileName

collapse :: FilePath -> FilePath
collapse path = joinPath $ collapse' $ splitDirectories path
  where collapse' [] = []
        collapse' (x : ".." : rest) = collapse' rest
        collapse' ("." : x : rest) = collapse' $ x : rest
        collapse' (x : rest) = x : collapse' rest

