{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Declarations (solidityContract) where

import Control.Monad

import Data.Either
import Data.List
import Data.Maybe

import Text.Parsec
import Text.Parsec.Perm

import Lexer
import SolidityParser
import SolidityTypes
import Types

solidityContract :: FileName -> SolidityParser ()
solidityContract fileName = do
  isLibrary <- (reserved "contract" >> return False) <|> (reserved "library" >> return True)
  name <- identifier
  initContract fileName name isLibrary
  optional $ do
    reserved "is"
    commaSep1 $ do
      name <- intercalate "." <$> sepBy1 identifier dot
      _ <- option "" parensCode
      addBase name -- Executed left-to-right, but reversed by prepending
    return ()
  braces $ skipMany solidityDeclaration
  isAbstract <- getIsAbstract

  if isAbstract && isLibrary
  then parserFail $ "Library " ++ name ++ " may not be an abstract contract"
  else return ()

solidityDeclaration :: SolidityParser ()
solidityDeclaration =
  structDeclaration <|>
  enumDeclaration <|>
  usingDeclaration <|>
  functionDeclaration <|>
  modifierDeclaration <|>
  eventDeclaration <|>
  variableDeclaration

{- New types -}

structDeclaration :: SolidityParser ()
structDeclaration = do
  reserved "struct"
  structName <- identifier
  structFields <- braces $ many1 $ do
    decl <- simpleVariableDeclaration [TypeStorage]
    semi
    return $ toFieldDef decl
  -- Reverse is necessary because the layout has to follow the convention
  -- of storage variables
  addType structName Struct{ fields = reverse structFields }

enumDeclaration :: SolidityParser ()
enumDeclaration = do
  reserved "enum"
  enumName <- identifier
  enumFields <- braces $ commaSep1 identifier
  addType enumName Enum{ names = enumFields }

-- This one has no type-level effect, so we don't have to do anything
usingDeclaration :: SolidityParser ()
usingDeclaration = do
  reserved "using"
  identifier
  reserved "for"
  optional $ identifier >> dot
  identifier
  semi
  return ()

{- Variables -}

variableDeclaration :: SolidityParser ()
variableDeclaration = do
  (vName, vDecl) <- simpleVariableDeclaration [StorageStorage, MemoryStorage, ValueStorage]
  optional $ do
    reservedOp "="
    many $ noneOf ";"
  semi
  when (null vName) $ fail "State variable name may not be empty"
  addVar vName vDecl

simpleVariableDeclaration :: [Storage] -> SolidityParser (Identifier, VarDef)
simpleVariableDeclaration allowedStorage = do
  variableType <- simpleTypeExpression
  typeMaker <- variableModifiers $ head allowedStorage
  variableName <- option "" identifier
  let result = (variableName, typeMaker variableType)
      stor = varStorage $ snd result
  unless (stor `elem` allowedStorage) $
    fail $ "Argument or variable named '" ++ variableName ++ 
           "' may not have storage " ++ show stor
  return result

{- Functions and function-like -}

functionDeclaration :: SolidityParser ()
functionDeclaration = do
  reserved "function"
  name <- option "" identifier
  args <- tupleDeclaration [MemoryStorage, StorageStorage]
  objMaker <- functionModifiers
  functionBody <- bracedCode <|> (semi >> return "")
  addFunc name $ objMaker args functionBody

eventDeclaration :: SolidityParser ()
eventDeclaration = do
  reserved "event"
  name <- identifier
  logs@(TupleValue logsL) <- tupleDeclaration [StorageStorage, IndexedStorage]
  when (length logsL > 4) $ fail $ "Event " ++ name ++ " has more than 4 topics"
  isAnon <- option False $ reserved "anonymous" >> return True
  semi
  addEvent name $ EventDef {
    eventTopics = logs,
    eventIsAnonymous = isAnon
    }

modifierDeclaration :: SolidityParser ()
modifierDeclaration = do
  reserved "modifier"
  name <- identifier
  args <- option (TupleValue []) $ tupleDeclaration [MemoryStorage]
  defn <- bracedCode
  addModifier name $ ModifierDef {
    modArgs = args,
    modDefn = defn
    }

{- Not really declarations -}

tupleDeclaration :: [Storage] -> SolidityParser Tuple
tupleDeclaration allowedStorage = 
  fmap TupleValue $ parens $ commaSep $ toArgDef <$>
  simpleVariableDeclaration allowedStorage

visibilityModifier :: SolidityParser Visibility
visibilityModifier =
  (reserved "public" >> return PublicVisible) <|>
  (reserved "private" >> return PrivateVisible) <|>
  (reserved "internal" >> return InternalVisible) <|>
  (reserved "external" >> return ExternalVisible)

storageModifier :: SolidityParser Storage
storageModifier =
  (reserved "constant" >> return ValueStorage) <|>
  (reserved "storage" >> return StorageStorage) <|>
  (reserved "memory" >> return MemoryStorage) <|>
  (reserved "indexed" >> return IndexedStorage)

variableModifiers :: Storage -> SolidityParser (BasicType -> VarDef)
variableModifiers defaultStorage =
  permute $ (\v s ->
    \variableType -> VarDef {
      varType = variableType,
      varVisibility = v,
      varStorage = s
    }) <$?>
    (InternalVisible, visibilityModifier) <|?>
    (defaultStorage, storageModifier)

functionModifiers :: SolidityParser (Tuple -> SourceCode -> FuncDef)
functionModifiers =
  permute $ (\r v s _ _ _ _ ->
    \args code -> FuncDef {
      funcValueType = r,
      funcArgType = args,
      funcVisibility = v,
      funcHasCode = not $ null code,
      funcIsConstructor = False, -- Will be changed in due course
      funcIsConstant = case s of {ValueStorage -> True; _ -> False}
    }) <$?>
    (TupleValue [], returnModifier) <|?>
    (PublicVisible, visibilityModifier) <|?>
    (MemoryStorage, storageModifier) <|?>
    ("", otherModifiers) <|?>
    ("", otherModifiers) <|?>
    ("", otherModifiers) <|?>
    ("", otherModifiers) -- Fenceposts for the explicit modifiers

  where
    returnModifier = reserved "returns" >> tupleDeclaration [MemoryStorage]
    otherModifiers = fmap (intercalate " ") $ many $ do
      name <- identifier
      args <- optionMaybe parensCode
      return $ name ++ maybe "" (\s -> "(" ++ s ++ ")") args

