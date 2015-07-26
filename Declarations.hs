module Declarations (solidityContract) where

import Data.Functor
import Data.Maybe
import Text.Parsec

import Lexer
import Modifiers
import ParserTypes
import Types

solidityContract :: SolidityParser SolidityContract
solidityContract = do
  reserved "contract"
  contractName <- identifier
  setContractName contractName
  skipMany $ noneOf "{"
  contractSymbols <- catMaybes <$> (braces $ many solidityDeclaration)
  return $ Contract contractName contractSymbols

-- Doesn't handle contract inheritance or contract types yet
solidityDeclaration :: SolidityParser (Maybe SoliditySymbol)
solidityDeclaration =
  structDeclaration <|>
  enumDeclaration <|>
  functionDeclaration <|>
  modifierDeclaration <|>
  eventDeclaration <|>
  variableDeclaration

structDeclaration :: SolidityParser (Maybe SoliditySymbol)
structDeclaration = do
  reserved "struct"
  structName <- identifier
  structFields <- catMaybes <$>
                  (braces $ many1 $ do
                      decl <- simpleVariableDeclaration
                      semi
                      return decl
                  )
  optional $
    reserved "memory" <|>
    reserved "storage" <|>
    reserved "calldata"
  addToTypeDefs structName (Struct structFields)
  return Nothing

enumDeclaration :: SolidityParser (Maybe SoliditySymbol)
enumDeclaration = do
  reserved "enum"
  enumName <- identifier
  enumFields <- braces $ commaSep1 identifier
  addToTypeDefs enumName (Enum enumFields)
  return Nothing

functionDeclaration :: SolidityParser (Maybe SoliditySymbol)
functionDeclaration = do
  reserved "function"
  functionName <- optionMaybe identifier
  functionArgs <- map fromJust <$> (parens $ commaSep simpleVariableDeclaration)
  functionRet <- functionModifiers -- Only handles "returns" for now
  _ <- bracedCode <|> (semi >> return ()) -- Doesn't handle function bodies yet
  contractName <- getContractName
  return $ 
    if isNothing functionName || fromJust functionName == contractName
    then Nothing
    else Just $ Function {
      funcName = fromJust functionName,
      args = functionArgs,
      returns = functionRet
      }

bracedCode :: SolidityParser ()
bracedCode = braces $ skipMany $ (skipMany1 $ noneOf "{}") <|> bracedCode

eventDeclaration :: SolidityParser (Maybe SoliditySymbol)
eventDeclaration = do
  reserved "event"
  _ <- identifier
  parens $ commaSep simpleVariableDeclaration
  optional $ reserved "anonymous"
  semi
  return Nothing    

variableDeclaration :: SolidityParser (Maybe SoliditySymbol)
variableDeclaration = do
  vDecl <- simpleVariableDeclaration <|> inferTypeDeclaration
  optional $ many $ noneOf ";" -- Doesn't handle assignments yet
  semi
  return vDecl

simpleVariableDeclaration :: SolidityParser (Maybe SoliditySymbol)
simpleVariableDeclaration = do
  variableType <- typeExpression
  variableName <- identifier
  return $
    (\vType -> 
      Variable {
        varName = variableName,
        varType = vType
        }
    ) <$> variableType

inferTypeDeclaration :: SolidityParser (Maybe SoliditySymbol)
inferTypeDeclaration = do
  reserved "var"
  _ <- identifier
  return Nothing -- This can't happen for a state variable

modifierDeclaration :: SolidityParser (Maybe SoliditySymbol)
modifierDeclaration = do
  reserved "modifier"
  _ <- identifier
  optional $ parens $ do
    t <- simpleTypeExpression
    optional identifier
    return $ Just t
  _ <- bracedCode
  return Nothing

typeExpression :: SolidityParser (Maybe SolidityType)
typeExpression = do
  theType <- simpleTypeExpression
  modifier <- typeModifiers -- Only handles "constant" for now
  return $ maybe (Just theType) (const Nothing) modifier
