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

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Text as Text

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

parseToJSON :: Map FileName SourceCode -> FileName -> Either Value Value
parseToJSON sources name = do
  contracts <- first convertImportError $ doContractStructure sources name
  let layouts = doContractLayouts contracts
      results = Map.mapWithKey (contractABI results layouts) layouts
  return $ toJSON results

type ValueMap = Map DeclarationID Value

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
    (nonempty' (pair "vars") v) ++
    (nonempty' (pair "funcs") f) ++
    (nonempty' (pair "events") e) ++
    (nonempty' (pair "types") t) ++
    (nonempty' (pair "libraryTypes") tL) ++
    (nonempty' (pair "constr") c) ++
    (nonempty' (pair "library") l)
    where
      nonempty' p = nonempty p . m
      m = Map.fromList . Map.foldrWithKey (\dID v l -> makeDeclAssoc dID v : l) []
      makeDeclAssoc dID v = (declarationRealName dID, v)

contractABI :: Map ContractName ContractABI -> ContractsABIByName ->
               ContractName -> SolidityContractABI -> ContractABI
contractABI allABI allContracts name contract =
  ContractABI {
    contractVarsABI = varsABI name vars (contractStorageVars contract)
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
    makeVarAssoc WithPos{startPos, storage = vID} =
      (declName vID, varABI name startPos $ varType $ vars Map.! vID)

fieldsABI :: ContractName ->  [SolidityBasicType WithPos] -> ValueMap
fieldsABI name = Map.fromList . map makeFieldAssoc
  where
    makeFieldAssoc WithPos{startPos, storage = fieldDef} =
      (fieldName fieldDef, varABI name startPos $ fieldType fieldDef)

funcsABI :: ContractName -> Map DeclID (SolidityNewType WithPos) -> 
            Map Identifier SolidityFuncDef -> ValueMap
funcsABI name allTypesL funcs = Map.map (funcABI name allTypesL) funcs
              
eventsABI :: ContractName -> Map DeclID (SolidityNewType WithPos) -> 
             Map Identifier SolidityEventDef -> ValueMap
eventsABI name allTypesL events = Map.map (eventABI name allTypesL) events

typesABI :: ContractName -> Map Identifier (SolidityNewType WithPos) -> ValueMap
typesABI name = Map.map (typeABI name)

libTypesABI :: Map ContractName ContractABI -> [DeclID] -> Map ContractName ValueMap
libTypesABI allABIs lIDs =
  Map.filter (not . Map.null) $
  Map.mapWithKey (\k abi -> Map.intersection (contractTypesABI m) (idMap Map.! k)) allABIs

  where
    idMap =
      Map.fromList $
      map (\DeclID{declContract, declName} -> (declContract, Map.singleton declName ())) lIDs

constrABI :: ContractName -> Map Identifier SolidityFuncDef -> ValueMap
constrABI name funcs = maybe Map.empty (tupleABI . funcArgType) $ Map.lookup name funcs

tupleABI :: ContractName -> SolidityTuple -> ValueMap
tupleABI cName (TupleValue vars) = Map.fromList $ zipWith indexObjABI [0 :: Integer ..] vars
  where
    indexObjABI i var = (realName, object $ (pair "index" i) : basicTypeABI cName (varType var))
      where 
        realName = if null name then "#" ++ show i else name
        name = varName var

varABI :: ContractName -> Natural -> SolidityBasicType -> Value
varABI name startBytes var = object $ pair "atBytes" startBytes : basicTypeABI name (varType var)

funcABI :: ContractName -> Map DeclID (SolidityNewType WithPos) -> SolidityFuncDef -> Value
funcABI cName allTypesL func = object $
  [pair "selector" $ selector allTypesL (funcID func) name args] ++
  (nonempty (pair "args") $ tupleABI cName args) ++
  (nonempty (pair "vals") $ tupleABI cName vals)
  where
    name = funcName func
    args = funcArgType func
    vals = funcValueType func

eventABI :: ContractName -> Map DeclID (SolidityNewType WithPos) -> SolidityEventDef -> Value
eventABI cName allTypesL event = object $ [pair "topics" $ tupleABI cName topics] ++ sig
  where
    sig = 
      if eventIsAnonymous event
      then []
      else [pair "selector" $ selector allTypesL (eventID event) name topics]
    name = eventName event
    topics = eventTopics event

typeABI :: ContractName -> SolidityNewType WithPos -> Value
typeABI cName Struct{fields = WithSize{sizeOf, storage = fieldsL}}
  object [
    pair "type" "Struct",
    pair "bytes" $ toInteger sizeOf,
    pair "fields" $ fieldsABI cName fieldsL
    ]
typeABI _ Enum{names = WithSize{sizeOf, storage = namesL}}
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
basicTypeABI _ (FixedArray eT l) = [
  pair "type" "Array",
  pair "length" $ toInteger l,
  tpair "entry" eT
  ]
basicTypeABI _ (DynamicArray eT) = [
  pair "type" "Array",
  pair "dynamic" True,
  tpair "entry" eT
  ]
basicTypeABI _ (Mapping dT cT) = [
  pair "type" "Mapping",
  pair "dynamic" True,
  tpair "key" dT,
  tpair "value" cT
  ]
basicTypeABI cName (Typedef DeclID{declContract, declName}) =
  [pair "typedef" declName] ++
  if cName == declContract then [] else [pair "library" x]) 

pair :: (ToJSON a) => String -> a -> Pair
pair x y = (fromString x, toJSON y)

tpair :: String -> SolidityBasicType -> Pair
tpair x y = (fromString x, object $ basicTypeABI y)

nonempty :: (ToJSON a) => (Value -> Pair) -> a -> [Pair]
nonempty f x = nonempty' f $ toJSON x

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

