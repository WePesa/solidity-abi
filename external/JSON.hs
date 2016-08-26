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
import qualified Data.Set as Set
import Data.Set (Set)
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
      results = Map.mapWithKey (contractJSON results) layouts
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

contractABI :: Map ContractName ContractABI -> ContractName -> SolidityContractABI ->
               ContractABI
contractABI allABI name contract =
  ContractABI {
    contractVarsABI = varsABI vars (contractStorageVars contract)
    contractFuncsABI = funcsABI types funcs,
    contractEventsABI = eventsABI types (byName $ contractEvents contract),
    contractTypesABI = typesABI (byName $ contractTypes contract),
    contractLibraryTypesABI = libTypesABI allABI (contractLibraryTypes contract),
    contractConstrABI = constrABI name funcs,
    contractIsConcreteABI = contractIsConcrete contract,
    contractLibraryABI = contractIsLibrary contract
    }
  where
    vars = byID $ contractVars contract
    types = byID $ contractTypes contract
    funcs = (byName $ contractFuncs contract) `del` name `del` ""
    del = flip Map.delete

varsABI :: Map DeclID SolidityVarDef -> [WithPos DeclID] -> ValueMap
varsABI vars varIDs = Map.fromList $ map makeVarAssoc varIDs
  where
    makeVarAssoc WithPos{startPos, storage = vID} =
      (declName vID, varABI startPos $ vars Map.! vID)

funcsABI :: Map ContractName SolidityTypesLayout -> SolidityFuncs -> ValueMap
funcsABI allTypesL funcs = Map.map (funcABI allTypesL) funcs
              
eventsABI :: Map ContractName SolidityTypesLayout -> SolidityEvents -> ValueMap
eventsABI allTypesL events = Map.map (eventABI allTypesL) events

typesABI :: SolidityTypesLayout -> SolidityTypes -> ValueMap
typesABI = Map.intersectionWith typeABI

libTypesABI :: Map ContractName ContractABI -> SolidityLibraryTypes -> Map ContractName ValueMap
libTypesABI = Map.intersectionWith libTypeABI

constrABI :: ContractName -> SolidityFuncs -> ValueMap
constrABI name funcs = maybe Map.empty (tupleABI . funcArgType) constrM
  where constrM = Map.lookup name funcs

tupleABI :: SolidityTuple -> ValueMap
tupleABI (TupleValue vars) = Map.fromList $ zipWith indexObjABI [0 :: Integer ..] vars
  where
    indexObjABI i var = (realName, object $ (pair "index" i) : basicTypeABI (varType var))
      where 
        realName = if null name then "#" ++ show i else name
        name = varName var

varABI :: SolidityVarLayout -> SolidityVarDef -> Value
varABI varL var = object $ pair "atBytes" startBytes : basicTypeABI (varType var)
  where startBytes = toInteger $ varStartBytes varL

funcABI :: Map ContractName SolidityTypesLayout -> SolidityFuncDef -> Value
funcABI allTypesL func = object $
  [pair "selector" $ selector allTypesL (funcID func) name args] ++
  (nonempty (pair "args") $ tupleABI args) ++
  (nonempty (pair "vals") $ tupleABI vals)
  where
    name = funcName func
    args = funcArgType func
    vals = funcValueType func

eventABI :: Map ContractName SolidityTypesLayout -> SolidityEventDef -> Value
eventABI allTypesL event = object $ [pair "topics" $ tupleABI topics] ++ sig
  where
    sig = 
      if eventIsAnonymous event
      then []
      else [pair "selector" $ selector allTypesL (eventID event) name topics]
    name = eventName event
    topics = eventTopics event

typeABI :: SolidityTypeLayout -> SolidityTypeDef -> Value
typeABI (StructLayout fieldsL tB) TypeDef{typeDecl = (Struct fields)} =
  object [
    pair "type" "Struct",
    pair "bytes" $ toInteger tB,
    pair "fields" $ varsABI fieldsL $ makeVarsMap fields
    ]
typeABI (EnumLayout tB) TypeDef{typeDecl = (Enum names)} =
  object $ [
    pair "type" "Enum",
    pair "bytes" $ toInteger tB,
    pair "names" names
    ]
typeABI _ _ = error "Broken types layout or missing type definition"

libTypeABI :: ContractABI -> Set Identifier -> ValueMap
libTypeABI contractABI lTypeNames = Map.fromList $ Set.foldr getTypeABI [] lTypeNames
  where getTypeABI name = ((name, contractTypesABI contractABI Map.! name) :)

basicTypeABI :: SolidityBasicType -> [Pair]
basicTypeABI Boolean = [pair "type" "Bool"]
basicTypeABI Address = [pair "type" "Address"]
basicTypeABI (SignedInt b) = [
  pair "type" "Int",
  pair "signed" True,
  pair "bytes" $ toInteger b]
basicTypeABI (UnsignedInt b) = [
  pair "type" "Int",
  pair "bytes" $ toInteger b
  ]
basicTypeABI (FixedBytes b) = [
  pair "type" "Bytes",
  pair "bytes" $ toInteger b
  ]
basicTypeABI DynamicBytes = [
  pair "type" "Bytes",
  pair "dynamic" True
  ]
basicTypeABI String = [
  pair "type" "String",
  pair "dynamic" True
  ]
basicTypeABI (FixedArray eT l) = [
  pair "type" "Array",
  pair "length" $ toInteger l,
  tpair "entry" eT
  ]
basicTypeABI (DynamicArray eT) = [
  pair "type" "Array",
  pair "dynamic" True,
  tpair "entry" eT
  ]
basicTypeABI (Mapping dT cT) = [
  pair "type" "Mapping",
  pair "dynamic" True,
  tpair "key" dT,
  tpair "value" cT
  ]
basicTypeABI (Typedef name libM) = [
  pair "typedef" name
  ] ++ maybe [] (\x -> [pair "library" x]) libM

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

