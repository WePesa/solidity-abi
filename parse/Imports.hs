{-# LANGUAGE DeriveFunctor #-}
module Imports (
  ImportError(..),
  getImportDefs,
  validateImports,
  collapse  
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Monoid
import Data.Traversable
import System.FilePath

import ParserTypes

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
    }

-- Assumes all filenames are collapsed, and all imports are valid
getImportDefs :: FileName ->
                 Map FileName (Either ImportError (Map ContractName a)) ->
                 [(FileName, ImportAs)] ->
                 Either ImportError (Map ContractName a)
getImportDefs mainFileName fileDefsEither imports = do
  imported <- mapM getQualifiedImports imports
  return $ Map.unions imported

  where
    getQualifiedImports (fileName, importAs) = do
      fileDef <- fileDefsEither Map.! relImport
      let symbolDefsEither = Map.map Right fileDef
      changeNames symbolDefsEither
      where
        relImport = mainFilePath <//> fileName
        mainFilePath = takeDirectory mainFileName
        changeNames = case importAs of
          Unqualified -> sequence
          StarPrefix p -> sequence . Map.mapKeys ((p ++ ".") ++)
          Aliases as -> sequence . Map.fromList . flip map as . getSym
            where getSym m (k, x) = (x, getSymbolEither k m)
        getSymbolEither sym =
          Map.findWithDefault (Left $ MissingSymbol mainFileName sym relImport) sym

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
        collapse' (x : ".." : rest) = collapse' rest
        collapse' ("." : x : rest) = collapse' $ x : rest
        collapse' (x : rest) = x : collapse' rest

data ImportState a = Go a | Err ImportError | Done

instance (Monoid a) => Monoid (ImportState a) where
  mappend x@(Err _) _ = x
  mappend _ x@(Err _) = x
  mappend (Go x) (Go y) = Go (x <> y)
  mappend Done x = x
  mappend x Done = x
  mempty = Done

-- Assumes that the filenames are all collapsed
validateImports :: Map FileName SolidityFile -> Either ImportError (Map FileName SolidityFile)
validateImports files = slurp $ Map.map (Go . map fst . fileImports) files
  where
    slurp importStateMap =
      case combineState shiftedStateMap of
        Done -> Right files
        Err e -> Left e
        _ -> slurp shiftedStateMap
      where shiftedStateMap = shift importStateMap

    shift importStateMap = Map.mapWithKey shiftState importStateMap
      where
        shiftState mainFile (Go imports) = checkCycles $ combineState $ map getImport imports
          where 
            getImport fileName = Map.findWithDefault (Err $ MissingImport mainFile fileName) fileName importStateMap
            checkCycles x@(Go newImports) = 
              if mainFile `elem` newImports
              then Err $ ImportCycle mainFile
              else x
            checkCycles x = x
        shiftState _ x = x

    combineState :: (Foldable t, Monoid a) => t (ImportState a) -> ImportState a
    combineState = foldMap id

