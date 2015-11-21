module Parser (parse) where

import qualified Data.Set as Set
import Data.Map (Map)

import Text.Parsec
import Text.Parsec.Pos

import Declarations
import Lexer
import ParserTypes

parse :: (SourceName -> String) -> SourceName -> String
          -> Either ParseError (Set SolidityContract)
parse importReader sourceName sourceCode =
  Set.fromList $ runParser (solidityFile importReader) "" sourceName sourceCode

solidityFile :: (SourceName -> String) -> SolidityParser [SolidityContract]
solidityFile importReader = do
  whiteSpace
  files <- many (solidityImport <|> fmap [] solidityContract)
  eof
  return $ concat files

solidityImport :: (SourceName -> String) -> SolidityParser [SolidityContract]
solidityImport importReader =
  let saveFile = do
        curFile <- getInput
        curPos <- getPosition
        return (curFile, curPos)
      newFile name = do
        setPosition initialPos
        setInput $ importReader name
        solidityFile importReader
      restoreFile (fileIn, filePos) = 
        setPosition fileIn
        setInput filePos
  in do
    reserved "import"
    importName <- soliditySourceFilename
    thisFile <- saveFile
    importFile <- newFile importName
    restoreFile thisfile
    return importFile
  
soliditySourceFilename :: SolidityParser SourceName
soliditySourceFilename = stringLiteral
