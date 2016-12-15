-- |
-- Module: SolidityParser
-- Description: Specialized parsers for different Solidity constructs,
--   mostly declarations.
-- Maintainer: Ryan Reich <ryan@blockapps.net>
--
-- Almost all of these functions have null return type, and instead modify
-- the ongoing state (the contract under construction) of the
-- SolidityParser.
{-# LANGUAGE NamedFieldPuns #-}
module SolidityParser where

import qualified Data.Map as Map

import Data.Bifunctor
import Text.Parsec
import SolidityTypes

-- | A parser of source code whose state is the name of the current
-- contract.
type SolidityParser = Parsec SourceCode (ContractID, Contract 'AfterParsing)

-- | Initializes the parser state to be an empty contract with the given
-- filename and contract name.  The contract defaults to being a library as
-- an edge case. 
initContract :: FileName -> ContractName -> Bool -> SolidityParser ()
initContract fileName name isLibrary = 
  putState (ContractID fileName name, emptyContract{contractIsLibrary = isLibrary})

-- | Constructs a "declaration ID" out of the given identifier.  This ID is
-- file-independent, which is important for handling qualified or aliasing imports.
makeDeclID :: Identifier -> SolidityParser DeclID
makeDeclID id = do
  (cID, _) <- getState
  return DeclID{
    declContract = cID,
    declName = id
    }

-- | Adds a new variable with the given name and definition.  This is
-- automatically added to both contractVars and, if necessary,
-- contractStorageVars.  The distinction is important for the storage
-- layout.
addVar :: Identifier -> VarDef -> SolidityParser ()
addVar vName v = do
  vID <- makeDeclID vName
  let theError = duplicateError "variable" vID
  modifyState $ second $
    \c@Contract{contractVars = DeclarationsBy{byName, byID}, contractStorageVars} ->
      c{contractVars = DeclarationsBy{
          byName = Map.insertWith theError vName vID byName,
          byID = Map.insertWith theError vID v byID
          },
        contractStorageVars =
          if isStorageVar v
          then vID:contractStorageVars
          else contractStorageVars
       }

-- | Adds a new function of the given name and definition.  This has the
-- effect of toggling whether the contract is "concrete" (has no
-- unimplemented functions) or not.
addFunc :: Identifier -> FuncDef -> SolidityParser ()
addFunc fName f = do
  fID <- makeDeclID fName
  let 
    f' = f{funcIsConstructor = fName == contractName (declContract fID)}
    theError =
      error $ "Duplicate definition of " ++ (
                if null $ declName fID
                then "default function"
                else "function " ++ declName fID
              ) ++ " in contract " ++ contractName (declContract fID)
                ++ " in file " ++ contractFile (declContract fID)
  modifyState $ second $
    \c@Contract{contractFuncs = DeclarationsBy{byName, byID}, contractIsConcrete} ->
      c{contractFuncs = DeclarationsBy{
          byName = 
            if funcIsConstructor f' || null fName
            then byName
            else Map.insertWith theError fName fID byName,
          byID = Map.insertWith theError fID f' byID
          },
        contractIsConcrete = contractIsConcrete && funcHasCode f'
       }

-- | Adds a new event of the given name and definition.
addEvent :: Identifier -> EventDef -> SolidityParser ()
addEvent eName e = do
  eID <- makeDeclID eName
  let theError = duplicateError "event" eID
  modifyState $ second $
    \c@Contract{contractEvents = DeclarationsBy{byName, byID}} ->
      c{contractEvents = DeclarationsBy{
          byName = Map.insertWith theError eName eID byName,
          byID = Map.insertWith theError eID e byID
          }
       }

-- | Adds a new modifier of the given name and definition.
addModifier :: Identifier -> ModifierDef -> SolidityParser ()
addModifier mName m = do
  mID <- makeDeclID mName
  let theError = duplicateError "modifier" mID
  modifyState $ second $
    \c@Contract{contractModifiers = DeclarationsBy{byName, byID}} ->
      c{contractModifiers = DeclarationsBy{
          byName = Map.insertWith theError mName mID byName,
          byID = Map.insertWith theError mID m byID
          }
       }

-- | Adds a new type of the given name and definition.  The stage type is
-- 'AfterParsing because this is the entry that will hold once the parser
-- completes; it is not further modified until later.
addType :: Identifier -> NewType 'AfterParsing -> SolidityParser ()
addType tName t = do
  tID <- makeDeclID tName
  let theError = duplicateError "type" tID
  modifyState $ second $
    \c@Contract{contractTypes = DeclarationsBy{byName, byID}} ->
      c{contractTypes = DeclarationsBy{
          byName = Map.insertWith theError tName tID byName,
          byID = Map.insertWith theError tID t byID
          }
       }

-- | Adds a new base contract for inheritance.  This carefully specifies
-- that the base name must be looked up in the current file.
addBase :: ContractName -> SolidityParser ()
addBase n = do
  (cID, _) <- getState
  let baseID = cID{contractName = n} -- Base names must be in the same file 
  modifyState $ second $
    \c -> c{contractBases = baseID : contractBases c}

-- | Adds a new linkage reference of the given rough kind.  What kind of
-- link (library, inheritance, etc.) it turns out to be will not be known
-- until later.
newLinkage :: RoughLink -> SolidityParser LinkID
newLinkage linkT = do
  (cID, _) <- getState
  let linkID = LinkID { linkContract = cID, linkIs = linkT }
  modifyState $ second $
    \c -> c{contractLinkage = Map.insert linkID linkT $ contractLinkage c}
  return linkID

-- | Whether a contract is abstract (not concrete, i.e. has unimplemented
-- functions)
getIsAbstract :: SolidityParser Bool
getIsAbstract = not . contractIsConcrete . snd <$> getState

-- | Unified error function for the most common cases
duplicateError :: String -> DeclID -> a
duplicateError name dID =
  error $ "Duplicate definition of " ++ name ++ declName dID ++ " in contract " ++ contractName (declContract dID) ++ " in file " ++ contractFile (declContract dID)

-- | Converts a name and an unnamed variable definition into an argument
-- definition (arguments must always have names included in their type).
toArgDef :: (Identifier, VarDef) -> ArgDef
toArgDef (name, VarDef{varType, varStorage}) =
  ArgDef{argName = name, argType = varType, argStorage = varStorage}

-- | Converts a name and an unnamed variable definition into a struct field
-- definition (struct fields must always have names included in their
-- type).
toFieldDef :: (Identifier, VarDef) -> FieldDef
toFieldDef (name, VarDef{varType}) =
  FieldDef{fieldName = name, fieldType = varType}

