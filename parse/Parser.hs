module Parser where

import Text.Parsec

import File
import SolidityParser
import SolidityTypes

parseSolidity :: FileName -> SourceCode -> SolidityFile
parseSolidity fileName source = 
  either (error . show) id $
  runParser (solidityFile fileName) (ContractID "" "", emptyContract) fileName source

