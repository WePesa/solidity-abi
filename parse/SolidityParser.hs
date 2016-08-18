{-# LANGUAGE NamedFieldPuns #-}
module SolidityParser where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Bifunctor
import Text.Parsec
import SolidityTypes

type SolidityParser = Parsec SourceCode (ContractName, SolidityContract)

initContract :: ContractName -> Bool -> SolidityParser ()
initContract name isLibrary = 
  putState (name, emptyContract{contractIsLibrary = isLibrary})

makeDeclID :: Identifier -> SolidityParser DeclID
makeDeclID id = do
  (name, _) <- getState
  return DeclId{
    declContract = name,
    declName = id
    }

addVar :: Identifier -> SolidityVarDef -> SolidityParser ()
addVar vName v = do
  vID <- makeDeclID vName
  modifyState $ second $
    \c@Contract{contractVars@DeclarationsBy{byName, byID}, contractStorageVars} ->
      c{byName = Map.insertWith theError vName v byName,
        byID = Map.insertWith theError vID v byID
        contractStorageVars =
          case varStorage v of
            StorageStorage -> v:contractStorageVars,
            _ -> contractStorageVars
       }
  where theError = duplicateError "variable" vID

addFunc :: Identifier -> SolidityFuncDef -> SolidityParser ()
addFunc fName f = do
  fID <- makeDeclID fName
  modifyState $ second $
    \c@Contract{contractFuncs@DeclarationsBy{byName, byID}} ->
      c{byName = Map.insertWith (declName fName) f byName,
        byID = Map.insertWith theError fID f byID
       }
  where 
    theError =
      error $ "Duplicate definition of " ++ (
                if null $ declName fID
                then "default function"
                else "function " ++ declName fID
              ) ++ " in contract " ++ declContract fID

addEvent :: Identifier -> SolidityEventDef -> SolidityParser ()
addEvent eName e = do
  eID <- makeDeclID eName
  modifyState $ second $
    \c@Contract{contractEvents@DeclarationsBy{byName, byID}} ->
      c{byName = Map.insertWith theError (declName eName) e byName,
        byID = Map.insertWith theError eID e byID}
  where theError = duplicateError "event" eID

addModifier :: Identifier -> SolidityModifierDef -> SolidityParser ()
addModifier mName m = do
  mID <- makeDeclID mName
  modifyState $ second $
    \c@Contract{contractModifiers@DeclarationsBy{byName, byID}} ->
      c{byName = Map.insertWith theError (declName mName) m byName,
        byID = Map.insertWith theError mID m byID}
  where theError = duplicateError "modifier" mID

addType :: Identifier -> SolidityTypeDef -> SolidityParser ()
addType tName t = do
  tID <- makeDeclID tName
  modifyState $ second $
    \c@Contract{contractTypes@DeclarationsBy{byName, byID}} ->
      c{byName = Map.insertWith (declName tName) t byName,
        byID = Map.insertWith tID t byID}
  where theError = duplicateError "type" tID

addLibraryType :: ContractName -> Identifier -> SolidityParser ()
addLibraryType lName tName = modifyState $ second $
  \c -> c@{contractLibraryTypes = Set.insert tID $ contractLibraryTypes c}
  where tID = DeclID {declContract = lName, declName = tName}

addBase :: ContractName -> SolidityParser ()
addBase n = modifyState $ second $
  \c -> c{contractInherits = x : contractInherits c}

addExternalName :: ([ContractName], Identifier) -> SolidityParser ()
addExternalName pathname = modifyState $ second $
  \c -> c{contractExternalNames = Set.insert pathName $ contractExternalNames c}

getIsAbstract :: SolidityParser Bool
getIsAbstract = not . contractIsConcrete . snd <$> getState

duplicateError :: String -> DeclID -> a
duplicateError name id =
  error $ "Duplicate definition of " ++ name ++ declID id ++ " in contract " ++ declContract id

emptyContract :: SolidityContract
emptyContract = 
  Contract {
    contractStorageVars = [],
    contractVars = emptyDeclsBy,
    contractFuncs = emptyDeclsBy,
    contractEvents = emptyDeclsBy,
    contractModifiers = emptyDeclsBy,
    contractTypes = emptyDeclsBy,
    contractExternalNames = Set.empty,
    contractLibraryTypes = Set.empty,
    contractInherits = [],
    contractIsConcrete = True,
    contractIsLibrary = False
    }

emptyDeclsBy :: DeclarationsBy a
emptyDeclsBy =
  DeclarationsBy {
    byName = Map.empty,
    byID = Map.empty
    }

toArgDef :: (Identifier, SolidityVarDef) -> SolidityArgDef
toArgDef name VarDef{varType, varStorage} =
   ArgDef{argName = name, argType = varType, argStorage = varStorage}

toFieldDef :: (Identifier, SolidityVarDef) -> SolidityFieldDef
toFieldDef name VarDef{varType} =
  FieldDef{fieldName = name, fieldType = varType}

