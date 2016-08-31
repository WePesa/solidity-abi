{-# LANGUAGE NamedFieldPuns #-}
module SolidityParser where

import qualified Data.Map as Map

import Data.Bifunctor
import Text.Parsec
import SolidityTypes

type SolidityParser = Parsec SourceCode (ContractID, SolidityContract)

initContract :: FileName -> ContractName -> Bool -> SolidityParser ()
initContract fileName name isLibrary = 
  putState (ContractID fileName name, emptyContract{contractIsLibrary = isLibrary})

makeDeclID :: Identifier -> SolidityParser DeclID
makeDeclID id = do
  (cID, _) <- getState
  return DeclID{
    declContract = cID,
    declName = id
    }

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
    \c@Contract{contractFuncs = DeclarationsBy{byName, byID}} ->
      c{contractFuncs = DeclarationsBy{
          byName = 
            if funcIsConstructor f' || null fName
            then byName
            else Map.insertWith theError fName fID byName,
          byID = Map.insertWith theError fID f' byID
          }
       }

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

addType :: Identifier -> NewType -> SolidityParser ()
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

addBase :: ContractName -> SolidityParser ()
addBase n = do
  (cID, _) <- getState
  let baseID = cID{contractName = n} -- Base names must be in the same file 
  modifyState $ second $
    \c -> c(contractBases = baseID : contractBases c}

newLinkage :: LinkT 'Incomplete -> SolidityParser LinkID
newLinkage linkT = do
  (cID, _) <- getState
  let linkID = LinkID { linkContract = cID, linkName = linkT }
  modifyState $ second $
    \c -> c{contractLinkage = Map.insert linkID linkT $ contractLinkage c}
  return linkID

getIsAbstract :: SolidityParser Bool
getIsAbstract = not . contractIsConcrete . snd <$> getState

duplicateError :: String -> DeclID -> a
duplicateError name id =
  error $ "Duplicate definition of " ++ name ++ declName id ++ " in contract " ++ declContract id

toArgDef :: (Identifier, VarDef) -> ArgDef
toArgDef (name, VarDef{varType, varStorage}) =
  ArgDef{argName = name, argType = varType, argStorage = varStorage}

toFieldDef :: (Identifier, VarDef) -> FieldDef
toFieldDef (name, VarDef{varType}) =
  FieldDef{fieldName = name, fieldType = varType}

