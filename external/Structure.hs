module Structure where

import Files
import Inheritance

doContractStructure :: Map FileName SourceCode -> Either ImportError ContractsByID
doContractStructure sources = do
  parsedFiles <- first (error . show) $ sequence $ Map.mapWithKey parseSolidity sources
  contracts0 <- doImports parsedFiles
  doInheritance contracts0

