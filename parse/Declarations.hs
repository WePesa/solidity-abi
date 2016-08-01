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
  sequence $ option [] $ do
    reserved "is"
    commaSep1 $ do
      name <- intercalate "." <$> sepBy1 identifier dot
      consArgs <- option "" parensCode
      addBase name consArgs
  braces $ many solidityDeclaration

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

{- Variables -}

variableDeclaration :: SolidityParser ()
variableDeclaration = do
  vDecl <- simpleVariableDeclaration
  vDefn <- optionMaybe $ do
    reservedOp "="
    many $ noneOf ";"
  semi
  addObj $ maybe vDecl (\vD -> vDecl{objDefn = vD}) vDefn

simpleVariableDeclaration :: SolidityParser SolidityObjDef
simpleVariableDeclaration = do
  variableType <- simpleTypeExpression
  typeMaker <- variableModifiers
  variableName <- option "" identifier
  return $ (typeMaker variableType){objName = variableName}

{- Functions and function-like -}

functionDeclaration :: SolidityParser ()
functionDeclaration = do
  reserved "function"
  name <- option "" identifier
  args <- tupleDeclaration
  objMaker <- functionModifiers
  functionBody <- bracedCode <|> (semi >> return "")
  addObj $ (objMaker name args){objDefn = functionBody}

eventDeclaration :: SolidityParser ()
eventDeclaration = do
  reserved "event"
  name <- identifier
  logs <- tupleDeclaration
  optional $ reserved "anonymous"
  semi
  addObj $ ObjDef {
    objName = name,
    objValueType = NoValue,
    objArgType = logs,
    objDefn = ""
    }

modifierDeclaration :: SolidityParser ()
modifierDeclaration = do
  reserved "modifier"
  name <- identifier
  args <- option NoValue tupleDeclaration
  defn <- bracedCode
  addObj $ ObjDef {
    objName = name,
    objValueType = NoValue,
    objArgType = args,
    objDefn = defn
    }

{- Not really declarations -}

tupleDeclaration :: SolidityParser SolidityTuple
tupleDeclaration = fmap TupleValue $ parens $ commaSep $ simpleVariableDeclaration

visibilityModifier :: SolidityParser SolidityVisibility
visibilityModifier =
  (reserved "public" >> PublicVisible) <|>
  (reserved "private" >> PrivateVisible) <|>
  (reserved "internal" >> InternalVisible) <|>
  (reserved "external" >> ExternalVisible)

storageModifier :: SolidityParser SolidityStorage
storageModifier =
  (reserved "constant" >> return ConstantStorage) <|>
  (reserved "storage" >> return StorageStorage) <|>
  (reserved "memory" >> return MemoryStorage)

variableModifiers :: SolidityParser (SolidityTuple -> SolidityObjDef)
variableModifiers variableType =
  permute $ (\v s ->
    ObjDef {
      objName = "",
      objValueType = SingleValue variableType,
      objArgType = NoValue,
      objDefn = "",
      objVisibility = v,
      objStorage = s
    }) <$?>
    (InternalVisible, visibilityModifier) <|?>
    (StorageStorage, storageModifier)

functionModifiers :: SolidityParser (Identifier -> SolidityTuple -> SolidityObjDef)
functionModifiers name args =
  permute $ (\r v s _ _ _ _ ->
    ObjDef {
      objName = name,
      objValueType = r,
      objArgType = args,
      objDefn = "",
      objVisibility = v,
      objStorage = s
    })
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
