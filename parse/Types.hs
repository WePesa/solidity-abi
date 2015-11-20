module Types where

import Data.Bool
import Data.Functor
import Data.Maybe
import Text.Parsec

import Expression
import Lexer
import ParserTypes

simpleTypeExpression :: SolidityParser SolidityType
simpleTypeExpression = try arrayType <|> simpleType <|> mappingType

simpleType :: SolidityParser SolidityType
simpleType =
  simple "bool" Boolean <|>
  simple "address" Address <|>
  simple "string" String <|>
  bytes <|>
  intSuffixed "uint" UnsignedInt <|>
  intSuffixed "int"  SignedInt   <|>
  -- realSuffixed "ureal" UnsignedReal  <|>
  -- realSuffixed "real" SignedReal     <|>
  (do
      alias <- identifier
      --Just realType <- getFromTypeDefs alias -- Crashes if unknown
      return $ UserDefined alias)
  where
    simple name nameType = do
      reserved name
      return nameType
    bytes =
      simple "byte" (FixedBytes 1) <|>
      simple "bytes" DynamicBytes <|>
      (lexeme $ try $ do
        string "bytes"
        let sizesS = reverse $ map show [1 .. 32]
        size <- read <$> (choice $ map (try . string) sizesS)
        return $ FixedBytes size)
    intSuffixed base baseType = lexeme $ try $ do
      string base
      let sizesS = reverse $ map show [8, 16 .. 256]
      sizeM <- optionMaybe $ choice $ map (try . string) sizesS
      let size = read $ fromMaybe (head sizesS) sizeM
      return $ baseType (size `quot` 8) -- in bytes
    -- realSuffixed base baseType = lexeme $ try $ do
    --   string base
    --   suffixM <- optionMaybe $ choice $ map try
    --              [ string ((show n) ++ "x" ++ (show m)) >> return (size, m) |
    --                n <- reverse [0, 8 .. 256],
    --                m <- reverse [0, 8 .. 256 - n],
    --                let size = n + m, size /= 0]
    --   return $ uncurry baseType $
    --     maybe (32,16) (\(s,m) -> (s `quot` 8, m `quot` 8)) suffixM -- in bytes

arrayType :: SolidityParser SolidityType
arrayType = do
  baseElemType <- simpleType <|> mappingType
  sizeList <- many1 $ brackets $ optionMaybe intExpr
  return $ makeArrayType baseElemType sizeList
  where
    makeArrayType = foldl (\t -> maybe (DynamicArray t) (FixedArray t))

mappingType :: SolidityParser SolidityType
mappingType = do
  reserved "mapping"
  (mapDomT, mapCodT) <- parens $ do
    d <- simpleTypeExpression
    reservedOp "=>"
    c <- simpleTypeExpression
    return (d, c)
  return $ Mapping mapDomT mapCodT
