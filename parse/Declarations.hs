-- |
-- Module: Declarations
-- Description: Parsers for top-level Solidity declarations
-- Maintainer: Ryan Reich <ryan@blockapps.net
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

-- | Parses an entire Solidity contract
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

-- | Parses anything that a contract can declare at the top level: new types,
-- variables, functions primarily, also events and function modifiers.
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

-- | Parses a struct definition
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

-- | Parses an enum definition
enumDeclaration :: SolidityParser ()
enumDeclaration = do
  reserved "enum"
  enumName <- identifier
  enumFields <- braces $ commaSep1 identifier
  addType enumName Enum{ names = enumFields }

-- | This one has no type-level effect, so we don't have to do anything
usingDeclaration :: SolidityParser ()
usingDeclaration = do
  reserved "using"
  identifier
  reserved "for"
  optional $ try (identifier >> dot)
  identifier
  semi
  return ()

{- Variables -}

-- | Parses a variable definition
variableDeclaration :: SolidityParser ()
variableDeclaration = do
  (vName, vDecl) <- simpleVariableDeclaration [StorageStorage, MemoryStorage, ValueStorage]
  optional $ do
    reservedOp "="
    many $ noneOf ";"
  semi
  when (null vName) $ fail "State variable name may not be empty"
  addVar vName vDecl

-- | Parses the declaration part of a variable definition, which is
-- everything except possibly the initializer and semicolon.  Necessary
-- because these kinds of expressions also appear in struct definitions and
-- function arguments.
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

-- | Parses a function definition.
functionDeclaration :: SolidityParser ()
functionDeclaration = do
  reserved "function"
  name <- option "" identifier
  args <- tupleDeclaration [MemoryStorage, StorageStorage]
  objMaker <- functionModifiers
  functionBody <- bracedCode <|> (semi >> return "")
  addFunc name $ objMaker args functionBody

-- | Parses an event definition.  At the moment we don't do anything with
-- it, but this prevents the parser from rejecting contracts that use
-- events.
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

-- | Parses a function modifier definition.  At the moment we don't do
-- anything with it, but this prevents the parser from rejecting contracts
-- that use modifiers.
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

-- | Parses a '(x, y, z)'-style tuple, such as appears in function
-- arguments and return values.  The argument is which storage types are
-- allowed in the tuple entries (as this depends on whether the tuple is
-- function arguments, function return values, event arguments, or modifier
-- arguments).
tupleDeclaration :: [Storage] -> SolidityParser Tuple
tupleDeclaration allowedStorage = 
  fmap TupleValue $ parens $ commaSep $ toArgDef <$>
  simpleVariableDeclaration allowedStorage

-- | Parses the variable visibility modifiers to internal type
visibilityModifier :: SolidityParser Visibility
visibilityModifier =
  (reserved "public" >> return PublicVisible) <|>
  (reserved "private" >> return PrivateVisible) <|>
  (reserved "internal" >> return InternalVisible) <|>
  (reserved "external" >> return ExternalVisible)

-- | Parses the variable storage modifiers to internal type
storageModifier :: SolidityParser Storage
storageModifier =
  (reserved "constant" >> return ValueStorage) <|>
  (reserved "storage" >> return StorageStorage) <|>
  (reserved "memory" >> return MemoryStorage) <|>
  (reserved "indexed" >> return IndexedStorage)

-- | Parses all the things that can modify a variable declaration, which
-- (fortunately, compared to functions) is just the visibility and storage
-- modifiers.
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

-- | Parses all the things that can modify a function declaration,
-- including return value, explicit function modifiers, visibility and
-- constant specifiers, and possibly base construtor arguments, in the case
-- of a constructor.  These can appear in any order, so we have to use
-- a special permutation parser for this.
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

