{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module File (solidityFile) where

import Data.Either
import Data.Map (fromList)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Pos

import Prelude hiding (lookup)

import Declarations
import Lexer
import ParserTypes

solidityFile :: FileName -> SolidityParser SolidityFile
solidityFile fileName = do
  whiteSpace
  toplevel <- many $ do
    let eitherImport = Right <$> solidityImport
        eitherContract = Left <$> (solidityContract fileName >> getState)
    eitherImport <|> eitherContract
  eof
  let (contracts, imports) = partitionEithers toplevel
      contracts' = map (
        \c@Contract{contractOwnDeclarations = cDs@{declaredVars = vs}} ->
          -- Vars were prepended in order, we want them appended in order
          c{contractOwnDeclarations = cDs{declaredVars = reverse vs}}
        ) contracts
      contractsAssoc = map (\c -> (contractRealName $ contractID c, c)) contracts'
  return SolidityFile {
    fileContracts = fromList contractsAssoc,
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
