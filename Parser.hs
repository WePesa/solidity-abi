module Parser (
  SolidityContract, SoliditySymbol(..), SolidityType(..),
  getABI, pretty
  ) where

import Control.Arrow
import Data.Bool
import Data.Functor
import qualified Data.Map.Strict as Map hiding (Map)
import Data.Map.Strict (Map)
import Data.Maybe
import Text.Parsec
import Text.Parsec.Language (javaStyle)
import Text.Parsec.Perm
import qualified Text.Parsec.Token as P

import ParserTypes

getABI :: SourceName -> String -> Either ParseError [SolidityContract]
getABI = runParser solidityFile ("", Map.empty)

solidityFile :: SolidityParser [SolidityContract]
solidityFile = do
  whiteSpace
  contracts <- many solidityContract
  eof
  return contracts

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
  addToTypeDefs structName (DeclaredType $ Struct structFields)
  return Nothing

enumDeclaration :: SolidityParser (Maybe SoliditySymbol)
enumDeclaration = do
  reserved "enum"
  enumName <- identifier
  enumFields <- braces $ commaSep1 identifier
  addToTypeDefs enumName (DeclaredType $ Enum enumFields)
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

functionModifiers :: SolidityParser (Maybe SolidityType)
functionModifiers =
  permute $
  -- All this explicit enumerate is ugly
  (\x _ _ _ _ _ _ _ _ _ _ _ _ -> x) <$?>
  (Nothing,
   do
     reserved "returns"
     parens $ do
       t <- simpleTypeExpression
       optional identifier
       return $ Just t) <|?>
  ((), reserved "public") <|?>
  ((), reserved "private") <|?>
  ((), reserved "internal") <|?>
  ((), reserved "external") <|?>
  ((), reserved "constant") <|?>
  modifiers <|?> modifiers <|?> modifiers <|?> modifiers <|?>
  modifiers <|?> modifiers <|?> modifiers -- 7 = 1 + # parsers before
  where
    modifiers = ([], many modifier)
    modifier = do
      modName <- identifier
      modValue <- optionMaybe $ parens $ many $ noneOf ")"
      return $ modName ++ "(" ++ fromMaybe "" modValue ++ ")"

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

typeExpression :: SolidityParser (Maybe SolidityType)
typeExpression = do
  theType <- simpleTypeExpression
  modifier <- typeModifiers -- Only handles "constant" for now
  return $ maybe (Just theType) (const Nothing) modifier

typeModifiers :: SolidityParser (Maybe ())
typeModifiers =
  permute $
  (\x _ _ _ _ _ _ -> x) <$?>
  (Nothing, Just <$> reserved "constant") <|?>
  ((), reserved "public") <|?>
  ((), reserved "private") <|?>
  ((), reserved "internal") <|?>
  ((), reserved "indexed") <|?>
  ((), reserved "storage") <|?>
  ((), reserved "memory")

simpleTypeExpression :: SolidityParser SolidityType
simpleTypeExpression = try arrayType <|> simpleType <|> mappingType

simpleType :: SolidityParser SolidityType
simpleType =
  simple "bool" Boolean <|>
  simple "address" Address <|>
  simple "byte" (Bytes 1) <|> -- Can handle this, and uint and int, with TypeDecls
  suffixed "bytes" Bytes [1 .. 32] <|>
  suffixed "uint" UnsignedInt [8, 16 .. 256] <|>
  suffixed "int"  SignedInt   [8, 16 .. 256] <|>
  (do
      alias <- identifier
      Just (DeclaredType realType) <- getFromTypeDefs alias -- Crashes if unknown
      return realType)
  where
    simple name nameType = do
      reserved name
      return nameType
    suffixed base baseType sizes = lexeme $ try $ do
      string base
      let sizesS = reverse $ map show sizes
      sizeM <- optionMaybe $ choice $ map (try . string) sizesS
      let size = read $ fromMaybe (head sizesS) sizeM
      return $ baseType size

arrayType :: SolidityParser SolidityType
arrayType = do
  baseElemType <- simpleType <|> mappingType
  sizeList <- many1 $ bracketCode
  return $ makeArrayType baseElemType sizeList
  where
    makeArrayType = foldl (\t -> bool (FixedArray t) (DynamicArray t))

bracketCode :: SolidityParser Bool
bracketCode = brackets $ null <$>
              (many $ (skipMany1 $ noneOf "[]") <|> (bracketCode >> return ()))

mappingType :: SolidityParser SolidityType
mappingType = do
  reserved "mapping"
  (mapDomT, mapCodT) <- parens $ do
    d <- simpleTypeExpression
    reservedOp "=>"
    c <- simpleTypeExpression
    return (d, c)
  return $ Mapping mapDomT mapCodT

reserved = P.reserved solidityLexer
reservedOp = P.reservedOp solidityLexer
identifier = P.identifier solidityLexer
lexeme = P.lexeme solidityLexer
natural = P.natural solidityLexer
braces = P.braces solidityLexer
parens = P.parens solidityLexer
brackets = P.brackets solidityLexer
commaSep = P.commaSep solidityLexer
commaSep1 = P.commaSep1 solidityLexer
semi = P.semi solidityLexer
semiSep = P.semiSep solidityLexer
semiSep1 = P.semiSep1 solidityLexer
whiteSpace = P.whiteSpace solidityLexer

solidityLexer = P.makeTokenParser solidityLanguage

solidityLanguage = javaStyle {
  P.reservedNames = [
     "contract", "is", "public", "internal", "private", "external", "import",
     "event", "indexed", "anonymous",
     "bool", "true", "false",
     "uint", "int", "bytes", "byte",
     "address", --"send", "balance",
     "enum", "struct", "mapping", "var",
     "function", "returns", "return", "modifier",
     "delete", "constant", "storage", "memory", "calldata",
     "if", "else", "while", "for", "break", "continue",
     "call", "callcode", "length", "sha3", "sha256", "ripemd160", "ecrecover",
     "suicide", "this",
     "block", --"coinbase", "difficulty", "gaslimit", "number", "blockhash", "timestamp",
     "msg", --"data", "gas", "sender", "value",
     "tx", --"gasprice", "origin",
     "wei", "finney", "szabo", "ether",
     "now", "seconds", "minutes", "hours", "days", "weeks", "years"
     ],
  P.reservedOpNames = [
    "!", "&&", "||", "==", "!=",
    "<=", ">=", "<", ">", "&", "|", "^", "~", "+", "*", "-", "/"," %", "**",
    "+=", "-=", "*=", "/=", "%=", "|=", "&=", "^=", "++", "--",
    "=>", "="
    ],
  P.caseSensitive = True,
  P.identStart = letter <|> char '_'
  }
  
type TypeDecls = Map String SolidityTypeExtended

type SolidityParser = Parsec String (String, TypeDecls)

setContractName :: String -> SolidityParser ()
setContractName name = putState =<< first (const name) <$> getState

getContractName :: SolidityParser String
getContractName = fst <$> getState

addToTypeDefs :: String -> SolidityTypeExtended -> SolidityParser ()
addToTypeDefs s t = putState =<< second (Map.insert s t) <$> getState

getFromTypeDefs :: String -> SolidityParser (Maybe SolidityTypeExtended)
getFromTypeDefs s = Map.lookup s . snd <$> getState  
