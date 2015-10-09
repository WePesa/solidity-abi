module Modifiers (typeModifiers, functionModifiers) where

import Data.Maybe
import Text.Parsec
import Text.Parsec.Perm

import Lexer
import ParserTypes
import Types

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
