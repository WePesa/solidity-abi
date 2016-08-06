module Parser (
  parseSolidity,
  FileName, Identifier, ContractName, SourceCode,
  ImportAs(..), SolidityFile(..),
  SolidityContract(..), SolidityVarDef(..),
  SolidityFuncDef(..), SolidityEventDef(..),
  SolidityModifierDef(..), 
  SolidityTypeDef(..), SolidityTuple(..),
  SolidityBasicType(..), SolidityNewType(..),
  SolidityVisibility(..), SolidityStorage(..)
  ) where

import Text.Parsec hiding (parse)

import File
import ParserTypes

parseSolidity :: FileName -> SourceCode -> Either ParseError SolidityFile
parseSolidity sName sCode = runParser solidityFile ("", emptyContract) sName sCode

