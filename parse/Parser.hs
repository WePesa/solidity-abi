module Parser (
  parseSolidity,
  FileName, Identifier, ContractName, SourceCode,
  ImportAs(..), SolidityFile(..), SolidityValue,
  SolidityContract(..), SolidityObjDef(..),
  SolidityTypeDef(..), SolidityTuple(..),
  SolidityBasicType(..), SolidityNewType(..)
  ) where

import Text.Parsec hiding (parse)

import File
import ParserTypes

parseSolidity :: FileName -> String -> Either ParseError SolidityFile
parseSolidity sName sCode = runParser solidityFile "" sName sCode

