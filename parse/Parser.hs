module Parser where

import Text.Parsec

import File
import SolidityParser
import SolidityTypes

parseSolidity :: FileName -> SourceCode -> Either ParseError SolidityFile
parseSolidity fileName source = 
  runParser (solidityFile fileName) ("", emptyContract) fileName source

