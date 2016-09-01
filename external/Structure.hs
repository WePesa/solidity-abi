module Structure (parseToStructure) where

import Data.Map (Map)
import qualified Data.Map as Map

import SolidityTypes
import Parser
import Inheritance
import Linkage
import Layout

parseToStructure :: FileName -> Map FileName SourceCode -> ContractsByName 'AfterLayout
parseToStructure name =
  Map.filter contractIsConcrete .
  getFileContracts name .
  doLayout .
  doLinkage .
  doInheritance .
  Map.mapWithKey parseSolidity

getFileContracts :: FileName -> ContractsByID 'AfterLayout -> ContractsByName 'AfterLayout
getFileContracts name = Map.mapKeys contractName . Map.filterWithKey isInFile
  where isInFile ContractID{contractFile} _ = name == contractFile

