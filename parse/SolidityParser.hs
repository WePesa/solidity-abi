{-# LANGUAGE NamedFieldPuns #-}
module SolidityParser where

import qualified Data.Map as Map

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
  return DeclID{
    declContract = name,
    declName = id
    }

addVar :: Identifier -> SolidityVarDef -> SolidityParser ()
addVar vName v = do
  vID <- makeDeclID vName
  let theError = duplicateError "variable" vID
  modifyState $ second $
    \c@Contract{contractVars = DeclarationsBy{byName, byID}, contractStorageVars} ->
      c{contractVars = DeclarationsBy{
          byName = Map.insertWith theError vName v byName,
          byID = Map.insertWith theError vID v byID
          },
        contractStorageVars =
          case varStorage v of
            StorageStorage -> vID:contractStorageVars
            _ -> contractStorageVars
       }

addFunc :: Identifier -> SolidityFuncDef -> SolidityParser ()
addFunc fName f = do
  fID <- makeDeclID fName
  let 
    theError =
      error $ "Duplicate definition of " ++ (
                if null $ declName fID
                then "default function"
                else "function " ++ declName fID
              ) ++ " in contract " ++ declContract fID
  modifyState $ second $
    \c@Contract{contractFuncs = DeclarationsBy{byName, byID}} ->
      c{contractFuncs = DeclarationsBy{
          byName = Map.insertWith theError fName f byName,
          byID = Map.insertWith theError fID f byID
          }
       }

addEvent :: Identifier -> SolidityEventDef -> SolidityParser ()
addEvent eName e = do
  eID <- makeDeclID eName
  let theError = duplicateError "event" eID
  modifyState $ second $
    \c@Contract{contractEvents = DeclarationsBy{byName, byID}} ->
      c{contractEvents = DeclarationsBy{
          byName = Map.insertWith theError eName e byName,
          byID = Map.insertWith theError eID e byID
          }
       }

addModifier :: Identifier -> SolidityModifierDef -> SolidityParser ()
addModifier mName m = do
  mID <- makeDeclID mName
  let theError = duplicateError "modifier" mID
  modifyState $ second $
    \c@Contract{contractModifiers = DeclarationsBy{byName, byID}} ->
      c{contractModifiers = DeclarationsBy{
          byName = Map.insertWith theError mName m byName,
          byID = Map.insertWith theError mID m byID
          }
       }

addType :: Identifier -> SolidityNewType -> SolidityParser ()
addType tName t = do
  tID <- makeDeclID tName
  let theError = duplicateError "type" tID
  modifyState $ second $
    \c@Contract{contractTypes = DeclarationsBy{byName, byID}} ->
      c{contractTypes = DeclarationsBy{
          byName = Map.insertWith theError tName t byName,
          byID = Map.insertWith theError tID t byID
          }
       }

addBase :: ContractName -> SolidityParser ()
addBase n = modifyState $ second $
  \c -> c{contractInherits = n : contractInherits c}

addExternalName :: ([ContractName], Identifier) -> SolidityParser ()
addExternalName pathName = modifyState $ second $
  \c -> c{contractExternalNames = pathName : contractExternalNames c}

getIsAbstract :: SolidityParser Bool
getIsAbstract = not . contractIsConcrete . snd <$> getState

duplicateError :: String -> DeclID -> a
duplicateError name id =
  error $ "Duplicate definition of " ++ name ++ declName id ++ " in contract " ++ declContract id

toArgDef :: (Identifier, SolidityVarDef) -> SolidityArgDef
toArgDef (name, VarDef{varType, varStorage}) =
   ArgDef{argName = name, argType = varType, argStorage = varStorage}

toFieldDef :: (Identifier, SolidityVarDef) -> SolidityFieldDef
toFieldDef (name, VarDef{varType}) =
  FieldDef{fieldName = name, fieldType = varType}

