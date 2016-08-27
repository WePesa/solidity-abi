{-# LANGUAGE NamedFieldPuns #-}
module JSON (parseToJSON) where

import Data.Aeson hiding (String)
import qualified Data.Aeson as Aeson (Value(String))
import Data.Aeson.Types (Pair)
import Data.Bifunctor
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import Data.Maybe
import Data.String

import Numeric.Natural

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Text as Text

import Files (ImportError(..))
import Layout
import Parser
import SolidityTypes
import Selector
import Structure

convertImportError :: ImportError -> Value
convertImportError (ImportCycle fBase) =
  object [pair "importError" "importCycle", 
          pair "inFile" fBase]
convertImportError (MissingImport fBase fName) = 
  object [pair "importError" "missingImport",
          pair "missingImport" fName,
          pair "inFile" fBase]
convertImportError (MissingSymbol fBase symName fName) = 
  object [pair "importError" "missingSymbol", 
          pair "missingSymbol" symName, 
          pair "fileName" fName, 
          pair "inFile" fBase]
convertImportError (DuplicateSymbol fBase fName symName) = 
  object [pair "importError" "duplicateSymbol", 
          pair "fileName" fName, 
          pair "duplicateSymbol" symName, 
          pair "inFile" fBase]
convertImportError (DuplicateFile fName) = 
  object [pair "importError" "duplicateFile", 
          pair "fileName" "fName"]

parseToJSON :: FileName -> Map FileName SourceCode -> Either Value Value
parseToJSON name sources = do
  contracts <- first convertImportError $ doContractStructure sources name
  let layouts = doContractLayouts contracts
      results = Map.mapWithKey (contractABI results layouts) layouts
  return $ toJSON results

type ValueMap = Map Identifier Value

data ContractABI =
  ContractABI {
    contractVarsABI :: ValueMap,
    contractFuncsABI :: ValueMap,
    contractEventsABI :: ValueMap,
    contractTypesABI :: ValueMap, 
    contractLibraryTypesABI :: Map ContractName ValueMap,
    contractConstrABI :: ValueMap,
    contractIsConcreteABI :: Bool,
    contractLibraryABI :: Bool
    }

instance ToJSON ContractABI where
  toJSON (ContractABI v f e t tL c _ l) = object $ 
    (nonempty "vars" v) ++
    (nonempty "funcs" f) ++
    (nonempty "events" e) ++
    (nonempty "types" t) ++
    (nonempty "libraryTypes" $ Map.mapWithKey nonempty tL) ++
    (nonempty "constr" c) ++
    (nonempty "library" l)

contractABI :: Map ContractName ContractABI -> ContractsABIByName ->
               ContractName -> SolidityContractABI -> ContractABI
contractABI allABI allContracts name contract =
  ContractABI {
    contractVarsABI = varsABI name vars (contractStorageVars contract),
    contractFuncsABI = funcsABI name allTypes funcs,
    contractEventsABI = eventsABI name allTypes (byName $ contractEvents contract),
    contractTypesABI = typesABI name (byName $ contractTypes contract),
    contractLibraryTypesABI = libTypesABI allABI (contractLibraryTypes contract),
    contractConstrABI = constrABI name funcs,
    contractIsConcreteABI = contractIsConcrete contract,
    contractLibraryABI = contractIsLibrary contract
    }
  where
    vars = byID $ contractVars contract
    allTypes = Map.foldr Map.union Map.empty $ Map.map (byID . contractTypes) allContracts
    funcs = (byName $ contractFuncs contract) `del` name `del` ""
    del = flip Map.delete

varsABI :: ContractName -> Map DeclID SolidityVarDef -> [WithPos DeclID] -> ValueMap
varsABI name vars varIDs = Map.fromList $ map makeVarAssoc varIDs
  where
    makeVarAssoc WithPos{startPos, stored = vID} =
      (declName vID, varABI name startPos $ varType $ vars Map.! vID)

fieldsABI :: ContractName -> [WithPos SolidityFieldDef] -> ValueMap
fieldsABI name = Map.fromList . map makeFieldAssoc
  where
    makeFieldAssoc WithPos{startPos, stored = fieldDef} =
      (fieldName fieldDef, varABI name startPos $ fieldType fieldDef)

funcsABI :: ContractName -> Map DeclID SolidityNewTypeABI -> 
            Map Identifier SolidityFuncDef -> ValueMap
funcsABI name allTypesL funcs = Map.mapWithKey (funcABI name allTypesL) funcs
              
eventsABI :: ContractName -> Map DeclID SolidityNewTypeABI -> 
             Map Identifier SolidityEventDef -> ValueMap
eventsABI name allTypesL events = Map.mapWithKey (eventABI name allTypesL) events

typesABI :: ContractName -> Map Identifier SolidityNewTypeABI -> ValueMap
typesABI name = Map.map (typeABI name)

libTypesABI :: Map ContractName ContractABI -> [DeclID] -> Map ContractName ValueMap
libTypesABI allABIs lIDs =
  Map.filter (not . Map.null) $
  Map.mapWithKey (\k abi -> Map.intersection (contractTypesABI abi) (idMap Map.! k)) allABIs

  where
    idMap =
      Map.fromList $
      map (\DeclID{declContract, declName} -> (declContract, Map.singleton declName ())) lIDs

constrABI :: ContractName -> Map Identifier SolidityFuncDef -> ValueMap
constrABI name funcs = maybe Map.empty (tupleABI name . funcArgType) $ Map.lookup name funcs

tupleABI :: ContractName -> SolidityTuple -> ValueMap
tupleABI cName (TupleValue args) = Map.fromList $ zipWith indexObjABI [0 :: Integer ..] args
  where
    indexObjABI :: Integer -> SolidityArgDef -> (Identifier, Value)
    indexObjABI i arg = (realName, object $ (pair "index" i) : basicTypeABI cName (argType arg))
      where 
        realName = if null name then "#" ++ show i else name
        name = argName arg

varABI :: ContractName -> Natural -> SolidityBasicType -> Value
varABI name startBytes t = object $ pair "atBytes" startBytes' : basicTypeABI name t
  where startBytes' = toInteger startBytes

funcABI :: ContractName -> Map DeclID SolidityNewTypeABI -> Identifier ->
           SolidityFuncDef -> Value
funcABI cName allTypesL name func = object $
  [pair "selector" $ selector allTypesL name args] ++
  (nonempty "args" $ tupleABI cName args) ++
  (nonempty "vals" $ tupleABI cName vals)
  where
    args = funcArgType func
    vals = funcValueType func

eventABI :: ContractName -> Map DeclID SolidityNewTypeABI -> Identifier -> 
            SolidityEventDef -> Value
eventABI cName allTypesL name event = object $ [pair "topics" $ tupleABI cName topics] ++ sig
  where
    sig = 
      if eventIsAnonymous event
      then []
      else [pair "selector" $ selector allTypesL name topics]
    topics = eventTopics event

typeABI :: ContractName -> SolidityNewTypeABI -> Value
typeABI cName StructPos{fieldsPos = WithSize{sizeOf, stored = fieldsL}} =
  object [
    pair "type" "Struct",
    pair "bytes" $ toInteger sizeOf,
    pair "fields" $ fieldsABI cName fieldsL
    ]
typeABI _ EnumPos{namesPos = WithSize{sizeOf, stored = namesL}} =
  object $ [
    pair "type" "Enum",
    pair "bytes" $ toInteger sizeOf,
    pair "names" namesL
    ]
typeABI _ _ = error "Broken types layout or missing type definition"

basicTypeABI :: ContractName -> SolidityBasicType -> [Pair]
basicTypeABI _ Boolean = [pair "type" "Bool"]
basicTypeABI _ Address = [pair "type" "Address"]
basicTypeABI _ (SignedInt b) = [
  pair "type" "Int",
  pair "signed" True,
  pair "bytes" $ toInteger b]
basicTypeABI _ (UnsignedInt b) = [
  pair "type" "Int",
  pair "bytes" $ toInteger b
  ]
basicTypeABI _ (FixedBytes b) = [
  pair "type" "Bytes",
  pair "bytes" $ toInteger b
  ]
basicTypeABI _ DynamicBytes = [
  pair "type" "Bytes",
  pair "dynamic" True
  ]
basicTypeABI _ String = [
  pair "type" "String",
  pair "dynamic" True
  ]
basicTypeABI cName (FixedArray eT l) = [
  pair "type" "Array",
  pair "length" $ toInteger l,
  tpair cName "entry" eT
  ]
basicTypeABI cName (DynamicArray eT) = [
  pair "type" "Array",
  pair "dynamic" True,
  tpair cName "entry" eT
  ]
basicTypeABI cName (Mapping dT cT) = [
  pair "type" "Mapping",
  pair "dynamic" True,
  tpair cName "key" dT,
  tpair cName "value" cT
  ]
basicTypeABI cName (Typedef DeclID{declContract, declName}) =
  [pair "typedef" declName] ++
  if cName == declContract then [] else [pair "library" declContract]

pair :: (ToJSON a) => String -> a -> Pair
pair x y = (fromString x, toJSON y)

tpair :: ContractName -> String -> SolidityBasicType -> Pair
tpair cName x y = (fromString x, object $ basicTypeABI cName y)

nonempty :: (ToJSON a) => String -> a -> [Pair]
nonempty s x = nonempty' (pair s) $ toJSON x

nonempty' :: (Value -> Pair) -> Value -> [Pair]
nonempty' f ob@(Object o) =
  if HashMap.null o
  then []
  else [f ob]
nonempty' f ar@(Array a) =
  if Vector.null a
  then []
  else [f ar]
nonempty' f st@(Aeson.String s) =
  if Text.null s
  then []
  else [f st]
nonempty' f bl@(Bool b) =
  if b
  then [f bl]
  else []
nonempty' _ Null = []
nonempty' f x = [f x]

