module Structure where

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

