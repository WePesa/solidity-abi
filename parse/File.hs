{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module File (solidityFile) where

import Data.Either
import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Pos

import Prelude hiding (lookup)

import Declarations
import Lexer
import SolidityParser
import SolidityTypes

solidityFile :: FileName -> SolidityParser SolidityFile
solidityFile fileName = do
  whiteSpace
  toplevel <- many $ do
    let eitherImport = Right <$> solidityImport
        eitherContract = Left <$> (solidityContract fileName >> getState)
    eitherImport <|> eitherContract
  eof
  let (contractsAssoc, imports) = partitionEithers toplevel
  return SolidityFile {
    fileContracts = Map.mapKeys contractName $ Map.fromList contractsAssoc,
    fileImports = imports
    }
 
solidityImport :: SolidityParser (FileName, ImportAs)
solidityImport = do
  reserved "import"
  i <- simpleImport <|> es6Import
  semi
  return i
 
simpleImport :: SolidityParser (FileName, ImportAs)
simpleImport = do
  importName <- soliditySourceFilename
  importAs <- option Unqualified $ do
    lexeme $ string "as"
    StarPrefix <$> identifier
  return (importName, importAs)

es6Import :: SolidityParser (FileName, ImportAs)
es6Import = do
  importAs <- es6ImportAs
  lexeme $ string "from"
  importName <- soliditySourceFilename
  return (importName, importAs)

es6ImportAs :: SolidityParser ImportAs
es6ImportAs = 
  (do
    importAs <- es6As
    case importAs of
      ("*", "*") -> return Unqualified
      ("*", p) -> return $ StarPrefix p
      _ -> parserFail "ES6-style import without braces must import \"*\""
  ) <|>
  (braces $ do
    importsAs <- commaSep1 es6As
    return $ Aliases importsAs
  )

es6As :: SolidityParser (ContractName, ContractName)
es6As = do
  origName <- identifier <|> (lexeme $ string "*")
  newName <- option origName $ do
    lexeme $ string "as"
    identifier
  return (origName, newName)

soliditySourceFilename :: SolidityParser FileName
soliditySourceFilename = stringLiteral
