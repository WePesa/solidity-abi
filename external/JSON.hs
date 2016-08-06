{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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

import Files
import Inheritance
import Layout
import LayoutTypes
import Parser
import ParserTypes
import Selector

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

type ValueMap = Map Identifier Value

data ContractABI =
  ContractABI {
    contractVarsABI :: ValueMap,
    contractFuncsABI :: ValueMap,
    contractEventsABI :: ValueMap,
    contractTypesABI :: ValueMap, 
    contractLibraryTypesABI :: Map ContractName ValueMap,
    contractConstrABI :: ValueMap,
    contractLibraryABI :: Bool
    }

instance ToJSON ContractABI where
  toJSON (ContractABI v f e t tL c l) = object $ 
    (nonempty (pair "vars") v) ++
    (nonempty (pair "funcs") f) ++
    (nonempty (pair "events") e) ++
    (nonempty (pair "types") t) ++
    (nonempty (pair "libraryTypes") tL) ++
    (nonempty (pair "constr") c) ++
    (nonempty (pair "library") l)

parseToJSON :: FileName -> Map FileName SourceCode -> Either Value Value
parseToJSON fileName sources = do
  parsedFiles <- first (error . show) $ sequence $ Map.mapWithKey parseSolidity sources
  mainFile <- first convertImportError $ doImports fileName parsedFiles
  contracts <- return $ doInheritance mainFile
  layouts <- return $ doLayout contracts
  let results = Map.intersectionWith (contractABI results layouts) layouts contracts
  return $ toJSON results

contractABI :: Map ContractName ContractABI -> SolidityContractsLayout ->
               SolidityContractLayout -> SolidityContract -> ContractABI
contractABI allABI allLayouts layout contract =
  ContractABI {
    contractVarsABI = varsABI varsL vars,
    contractFuncsABI = funcsABI typesL allTypesL funcs,
    contractEventsABI = eventsABI typesL allTypesL events,
    contractTypesABI = typesABI typesL types,
    contractLibraryTypesABI = libTypesABI allABI lTypes,
    contractConstrABI = constrABI name funcs,
    contractLibraryABI = contractIsLibrary contract
    }
  where
    varsL = varsLayout layout 
    typesL = typesLayout layout 
    allTypesL = Map.map typesLayout allLayouts

    name = contractName contract
    vars = makeVarsMap $ contractVars contract
    funcs = contractFuncs contract `del` name `del` ""
    events = contractEvents contract
    types = contractTypes contract
    lTypes = contractLibraryTypes contract

    del = flip Map.delete

makeVarsMap :: [SolidityVarDef] -> SolidityVars
makeVarsMap = Map.fromList . map (\v -> (varName v, v))

varsABI :: SolidityVarsLayout -> SolidityVars -> ValueMap
varsABI = Map.intersectionWith varABI

funcsABI :: SolidityTypesLayout -> Map ContractName SolidityTypesLayout ->
            SolidityFuncs -> ValueMap
funcsABI typesL allTypesL funcs = Map.map (funcABI typesL allTypesL) funcs
              
eventsABI :: SolidityTypesLayout -> Map ContractName SolidityTypesLayout ->
             SolidityEvents -> ValueMap
eventsABI typesL allTypesL events = Map.map (eventABI typesL allTypesL) events

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

funcABI :: SolidityTypesLayout -> Map ContractName SolidityTypesLayout -> SolidityFuncDef -> Value
funcABI typesL allTypesL func = object $
  [pair "selector" $ selector typesL allTypesL name args] ++
  (nonempty (pair "args") $ tupleABI args) ++
  (nonempty (pair "vals") $ tupleABI vals)
  where
    name = funcName func
    args = funcArgType func
    vals = funcValueType func

eventABI :: SolidityTypesLayout -> Map ContractName SolidityTypesLayout -> SolidityEventDef -> Value
eventABI typesL allTypesL event = object $ [pair "topics" $ tupleABI topics] ++ sig
  where
    sig = 
      if eventIsAnonymous event
      then []
      else [pair "selector" $ selector typesL allTypesL name topics]
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

