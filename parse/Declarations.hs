{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Declarations (solidityContract) where

import Data.Either
import Data.List
import Data.Maybe

import Text.Parsec
import Text.Parsec.Perm

import Lexer
import ParserTypes
import Types

solidityContract :: SolidityParser ()
solidityContract = do
  (reserved "contract" >> return ()) <|> (reserved "library" >> setIsLibrary)
  setContractName =<< identifier
  optional $ do
    reserved "is"
    commaSep1 $ do
      name <- intercalate "." <$> sepBy1 identifier dot
      consArgs <- option "" parensCode
      addBase name consArgs
    return ()
  braces $ skipMany solidityDeclaration

-- TODO: move this to layout and json modules.  Effect varies.
--    contractObjs = filter (tupleHasValue . objValueType) contractObjs',

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
    decl <- simpleVariableDeclaration
    semi
    return decl
  addType $ TypeDef {
    typeName = structName,
    typeDecl = Struct { fields = structFields }
    }

enumDeclaration :: SolidityParser ()
enumDeclaration = do
  reserved "enum"
  enumName <- identifier
  enumFields <- braces $ commaSep1 identifier
  addType $ TypeDef {
    typeName = enumName,
    typeDecl = Enum { names = enumFields}
    }

-- This one has no type-level effect, so we don't have to do anything
usingDeclaration :: SolidityParser ()
usingDeclaration = do
  reserved "using"
  reserved "for"
  identifier
  semi
  return ()

{- Variables -}

variableDeclaration :: SolidityParser ()
variableDeclaration = do
  vDecl <- simpleVariableDeclaration
  vDefn <- optionMaybe $ do
    reservedOp "="
    many $ noneOf ";"
  semi
  addVar $ maybe vDecl (\vD -> vDecl{varAssignment = vD}) vDefn

simpleVariableDeclaration :: SolidityParser SolidityObjDef
simpleVariableDeclaration = do
  variableType <- simpleTypeExpression
  typeMaker <- variableModifiers
  variableName <- option "" identifier
  return $ (typeMaker variableType){varName = variableName}

{- Functions and function-like -}

functionDeclaration :: SolidityParser ()
functionDeclaration = do
  reserved "function"
  name <- option "" identifier
  args <- tupleDeclaration
  objMaker <- functionModifiers
  functionBody <- bracedCode <|> (semi >> return "")
  addFunc $ (objMaker name args){funcDefn = functionBody}

eventDeclaration :: SolidityParser ()
eventDeclaration = do
  reserved "event"
  name <- identifier
  logs <- tupleDeclaration
  isAnon <- option False $ (reserved "anonymous" >> True)
  semi
  addEvent $ EventDef {
    eventName = name,
    eventTopics = logs,
    eventIsAnonymous = isAnon
    }

modifierDeclaration :: SolidityParser ()
modifierDeclaration = do
  reserved "modifier"
  name <- identifier
  args <- option (TupleValue []) tupleDeclaration
  defn <- bracedCode
  addModifier $ ModifierDef {
    modName = name,
    modArgs = args,
    modDefn = defn
    }

{- Not really declarations -}

tupleDeclaration :: SolidityParser SolidityTuple
tupleDeclaration = fmap TupleValue $ parens $ commaSep $ simpleVariableDeclaration

visibilityModifier :: SolidityParser SolidityVisibility
visibilityModifier =
  (reserved "public" >> return PublicVisible) <|>
  (reserved "private" >> return PrivateVisible) <|>
  (reserved "internal" >> return InternalVisible) <|>
  (reserved "external" >> return ExternalVisible)

storageModifier :: SolidityParser SolidityStorage
storageModifier =
  (reserved "constant" >> return ValueStorage) <|>
  (reserved "storage" >> return StorageStorage) <|>
  (reserved "memory" >> return MemoryStorage) <|>
  (reserved "indexed" >> return IndexedStorage)

variableModifiers :: SolidityParser (SolidityBasicType -> SolidityVarDef)
variableModifiers =
  permute $ (\v s ->
    \variableType -> VarDef {
      varName = "",
      varType = variableType,
      varAssignment = "",
      varVisibility = v,
      varStorage = s
    }) <$?>
    (InternalVisible, visibilityModifier) <|?>
    (StorageStorage, storageModifier)

functionModifiers :: SolidityParser (Identifier -> SolidityTuple -> SolidityFuncDef)
functionModifiers =
  permute $ (\r v s _ _ _ _ ->
    \name args -> FuncDef {
      funcName = name,
      funcValueType = r,
      funcArgType = args,
      funcDefn = "",
      funcVisibility = v,
      funcStorage = case s of {ValueStorage -> True; _ -> False}
    }) <$?>
    (TupleValue [], returnModifier) <|?>
    (PublicVisible, visibilityModifier) <|?>
    (MemoryStorage, storageModifier) <|?>
    ("", otherModifiers) <|?>
    ("", otherModifiers) <|?>
    ("", otherModifiers) <|?>
    ("", otherModifiers) -- Fenceposts for the explicit modifiers

  where
    returnModifier = reserved "returns" >> tupleDeclaration
    otherModifiers = fmap (intercalate " ") $ many $ do
      name <- identifier
      args <- optionMaybe parensCode
      return $ name ++ maybe "" (\s -> "(" ++ s ++ ")") args
