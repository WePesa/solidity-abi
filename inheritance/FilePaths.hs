-- |
-- Module: FilePaths
-- Description: Modify all import requests to use sanitized file paths.
-- Maintainer: Ryan Reich <ryan@blockapps.net>
module FilePaths (fixAllPaths) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bifunctor
import System.FilePath

import SolidityTypes

-- | Fix all import paths:
--  - Make them relative to a common root
--  - Remove all dot paths
fixAllPaths :: SolidityFiles -> SolidityFiles
fixAllPaths = makeImportsRelative . mapKeysWithKey handleCollision collapse
  where
    handleCollision fn _ _ = 
      error $ "The file path " ++ fn ++ " refers to two different source files"
    mapKeysWithKey combine f m = 
      Map.fromListWithKey combine $ Map.foldrWithKey (\k x xs -> (f k, x) : xs) [] m

makeImportsRelative :: SolidityFiles -> SolidityFiles
makeImportsRelative = Map.mapWithKey $ modifyImportsBy . takeDirectory
  where modifyImportsBy fn f = f{fileImports = map (first (fn <//>)) $ fileImports f}

infixl 5 <//>
(<//>) :: FileName -> FileName -> FileName
mp <//> fn = collapse $ prependIfRelative mp fn

prependIfRelative :: FileName -> FileName -> FileName
prependIfRelative mp fn =
  case splitDirectories fn of
    "." : _ -> mp </> fn
    ".." : _ -> mp </> fn
    _ -> fn

collapse :: FileName -> FileName
collapse path = joinPath $ collapse' $ splitDirectories path
  where collapse' [] = []
        collapse' (_ : ".." : rest) = collapse' rest
        collapse' ("." : x : rest) = collapse' $ x : rest
        collapse' (x : rest) = x : collapse' rest

