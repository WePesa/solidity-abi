module Structure where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bifunctor

import Files
import Inheritance
import Parser
import SolidityTypes

doContractStructure :: Map FileName SourceCode -> FileName ->
                       Either ImportError ContractsByName
doContractStructure sources name = do
  parsedFiles <- first (error . show) $ sequence $ Map.mapWithKey parseSolidity sources
  contracts0 <- doImports parsedFiles name
  return $ doInheritance contracts0

