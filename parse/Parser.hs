module Parser (
  parse,
  Identifier, ContractName, SourceCode,
  SolidityFile, SolidityValue,
  SolidityContract(..), SolidityObjDef(..),
  SolidityTypeDef(..), SolidityTuple(..),
  SolidityBasicType(..), SolidityNewType(..)
  ) where

import qualified Data.Set as Set
import Data.Map (Map)

import Text.Parsec hiding (parse)
import Text.Parsec.Pos

import Declarations
import Lexer
import ParserTypes

parse :: (SourceName -> String) -> SourceName -> String
          -> Either ParseError SolidityFile
parse importReader sourceName sourceCode =
  runParser (solidityFile importReader) "" sourceName sourceCode

solidityFile :: (SourceName -> String) -> SolidityParser SolidityFile
solidityFile importReader = do
  whiteSpace
  files <- many (solidityImport importReader <|> fmap return solidityContract)
  eof
  return $ concat files

solidityImport :: (SourceName -> String) -> SolidityParser SolidityFile
solidityImport importReader =
  let saveFile = do
        curFile <- getInput
        curPos <- getPosition
        return (curFile, curPos)
      newFile name = do
        setPosition $ initialPos name
        setInput $ importReader name
        solidityFile importReader
      restoreFile (fileIn, filePos) = do
        setPosition filePos
        setInput fileIn
  in do
    reserved "import"
    importName <- soliditySourceFilename
    semi
    thisFile <- saveFile
    importFile <- newFile importName
    restoreFile thisFile
    return importFile
  
soliditySourceFilename :: SolidityParser SourceName
soliditySourceFilename = stringLiteral
