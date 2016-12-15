-- |
-- Module: Parser
-- Description: The Solidity source parser function
-- Maintainer: Ryan Reich <ryan@blockapps.net>
module Parser where

import Text.Parsec

import File
import SolidityParser
import SolidityTypes

-- | The 'parseSolidity' function parses a single Solidity source string (with
-- a given name) into a Haskell data type.parseSolidity :: FileName -> SourceCode -> SolidityFile
parseSolidity fileName source = 
  either (error . show) id $
  runParser (solidityFile fileName) (ContractID "" "", emptyContract) fileName source
