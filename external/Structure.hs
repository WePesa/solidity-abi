-- |
-- Module: Structure
-- Description: End-to-end function for parsing a Solidity file
-- Maintainer: Ryan Reich <ryan@blockapps.net>
module Structure (parseToStructure) where

import Data.Map (Map)
import qualified Data.Map as Map

import SolidityTypes
import Parser
import Inheritance
import Linkage
import Layout

-- | Takes a map of filenames to source code, and returns a map of
-- filenames to contract names to fully parsed contract types
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
  Map.map Map.fromList . Map.fromListWith (++) . Map.foldrWithKey pushAssoc []
  where pushAssoc cID c = ( (contractFile cID, [(contractName cID, c)]) : )

