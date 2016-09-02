module Structure (parseToStructure) where

import Data.Map (Map)
import qualified Data.Map as Map

import SolidityTypes
import Parser
import Inheritance
import Linkage
import Layout

parseToStructure :: Map FileName SourceCode -> ContractsByFile 'AfterLayout
parseToStructure =
  makeContractsByFile .
  Map.filter contractIsConcrete .
  doLayout .
  doLinkage .
  doInheritance .
  Map.mapWithKey parseSolidity

makeContractsByFile :: ContractsByID 'AfterLayout -> ContractsByFile 'AfterLayout
makeContractsByFile = 
  Map.map (Map.fromList) . Map.fromListWith (++) . Map.foldrWithKey pushAssoc []
  where pushAssoc cID c = ( (contractFile cID, [(contractName cID, c)]) : )

