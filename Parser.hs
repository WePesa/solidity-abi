module Parser (getABI) where

import qualified Data.Map as Map
import Text.Parsec

import Declarations
import Lexer
import ParserTypes

getABI :: SourceName -> String -> Either ParseError [SolidityContract]
getABI = runParser solidityFile emptyDefinitions

solidityFile :: SolidityParser [SolidityContract]
solidityFile = do
  whiteSpace
  contracts <- many $ do
    clearState
    solidityContract
  eof
  return contracts
