module Parser (getABI) where

import qualified Data.Map as Map
import Text.Parsec

import Declarations
import Lexer
import ParserTypes

getABI :: SourceName -> String
          -> Either ParseError [([SoliditySymbol],SolidityContract)]
getABI = runParser solidityFile emptyDefinitions

solidityFile :: SolidityParser [([SoliditySymbol], SolidityContract)]
solidityFile = do
  whiteSpace
  contracts <- many $ do
    clearState
    c <- solidityContract
    s <- getState
    let declToSymbol (dName, dType) = Variable dName dType
        decls = map declToSymbol $ Map.toList $ contractTypes s
    return (decls, c)
  eof
  return contracts
