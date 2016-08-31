{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ViewPatterns, GeneralizedNewTypeInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JSON () where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Aeson
import qualified Data.Aeson as Aeson (String)
import qualified Data.HashMap as HashMap
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Control.Monad.Trans.Reader
import Data.Monoid
import Numeric.Natural

import SolidityTypes
import Selector

instance ToJSON (Contract 'AfterLayout) where
  toJSON = object . contractToJSON
  toEncoding = pairs . contractToJSON

newtype AssocJSON = AssocJSON {assocJSON :: [Pair]} deriving (Monoid)

instance KeyValue AssocJSON where
  (.=) k (toJSON -> Object x) | HashMap.null x = AssocJSON []
  (.=) k (toJSON -> Array x) | Vector.null x = AssocJSON []
  (.=) k (toJSON -> String x) | Text.null x = AssocJSON []
  (.=) k (toJSON -> Null) = AssocJSON []
  (.=) k (toJSON -> v) = AssocJSON [(k, v)]

instance ToJSON AssocJSON where
  toJSON = object . assocJSON

contractToJSON :: (KeyValue kv) => Contract 'AfterLayout -> kv
contractToJSON c = 
  "vars" .= 
    Map.map 
      (storageMap Map.!)
      (Map.filter isStorageVar $ defsByName $ contractVars c)
      <>
  "funcs" .= Map.mapWithKey (funcToJSON typesLinkage) (defsByName $ contractFuncs c) <>
  "events" .= Map.mapWithKey (eventToJSON typesLinkage) (defsByName $ contractEvents c) <>
  "types" .= 
    Map.map 
      (typeToJSON typesLinkage) 
      (Map.mapKeys showDeclJSON $ byID $ contractTypes c) 
      <>
  "constr" .= (tupleToJSON . funcArgType) <$> (Map.lookup constrID $ bID $ contractFuncs c) <>
  "library" .= contractIsLibrary c <>
  "libraries" .= map contractName librariesLinkage <>
  "storage" .= map (storageVarToJSON . fmap varType) (contractStorageVars c)

  where
    storageMap = Map.fromList $ zip (map stored $ contractStorageVars c) [0..]
    defsByName declsBy = Map.map (byID declsBy Map.!) (byName declsBy)
    CompleteLinkage{typesLinkage, librariesLinkage} = contractLinkage c
    constrID = DeclID{declContract = cID, declName = contractName cID}
    cID = NonEmpty.head $ allBases $ contractBases c

type JSONReader = Reader (Linkage 'AfterLayout)

funcToJSON :: (ToJSON kv, KeyValue kv) => Identifier -> FuncDef -> JSONReader kv
funcToJSON name func{funcArgType, funcValueType} = do
  linkage <- ask
  aJSON <- tupleToJSON funcArgType
  vJSON <- tupleToJSON funcValueType
  return $
    "selector" .= selector linkage name funcArgType <>
    "args" .= aJSON <>
    "vals" .= vJSON

eventToJSON :: (ToJSON kv, KeyValue kv) => Identifier -> EventDef -> JSONReader kv
eventToJSON name event{eventTopics, eventIsAnonymous} = do
  linkage <- ask
  tJSON <- tupleToJSON eventTopics
  return $
    "selector" .= 
      (if eventIsAnonymous then Nothing else Just $ selector linkage name eventTopics) <>
    "topics" .= tJSON

typeToJSON :: (ToJSON kv, KeyValue kv) => NewType 'AfterLayout -> JSONReader kv
typeToJSON Enum{names} = return $
  "type" .= "Enum" <>
  "bytes" .= toInteger $ sizeOf names <>
  "names" .= stored names
typeToJSON Struct{fields = WithSize{sizeOf, stored}} = do
  fJSON <- sequence $ Map.map storageVarToJSON stored
  return $
    "type" .= "Struct" <>
    "bytes" .= toInteger sizeOf <>
    "fields" .= fJSON

tupleToJSON :: (ToJSON kv, KeyValue kv) => Tuple -> JSONReader kv
tupleToJSON (TupleValue argsDef) = do
  typesJSON <- mapM makeTypeJSON argsDef
  return $ Map.fromList $ zipWith makeIndexAssoc [0..] typesJSON $ map argName argsDef 

  where
    makeTypeJSON ArgDef{argType, argStorage} = do
      aJSON <- basicTypeJSON argType
      return $
        "indexed" .= argStorage == IndexedStorage <>
        aJSON
    makeIndexAssoc i aJSON name = (
      if null name then "#" ++ show i else name, 
      "index" .= i <>
      aJSON
      )

storageVarToJSON :: (ToJSON kv, KeyValue kv) => WithPos BasicType -> JSONReader kv
storageVarToJSON linkage varDef{startPos, stored} = do
  typeABI <- basicTypeJSON linkage stored
  return $
    "atBytes" .= toInteger startPos <>
    typeABI

basicTypeJSON :: (ToJSON kv, KeyValue kv) => BasicType -> JSONReader kv
basicTypeJSON t = case t of
  Boolean -> return $
    "type" .= "Bool"
  Address -> return $ 
    "type" .= "Address"
  SignedInt b -> return $ 
    "type" .= "Int" <>
    "signed" .= True <>
    "bytes" .= toInteger b
  UnsignedInt b -> return $
    "type" .= "Int" <>
    "bytes" .= toInteger b
  FixedBytes b -> return $
    "type" .= "Bytes" <>
    "bytes" .= toInteger b
  DynamicBytes b -> return $
    "type" .= "Bytes" <>
    "dynamic" .= True
  String -> return $
    "type" .= "String" <>
    "dynamic" .= True
  FixedArray eT l -> do
    eJSON <- basicTypeABI eT
    return $ 
      "type" .= "Array" <>
      "length" .= toInteger l <>
      "entry" .= eJSON
  DynamicArray eT -> do
    eJSON <- basicTypeABI eT
    return $ 
      "type" .= "Array" <>
      "dynamic" .= True <>
      "entry" .= eJSON
  Mapping dT cT -> do
    kJSON <- basicTypeABI dT
    vJSON <- basicTypeABI cT
    return $
      "type" .= "Mapping" <>
      "dynamic" .= True <>
      "key" .= kJSON <>
      "value" .= vJSON
  LinkT linkID -> do
    linkage <- ask
    return $ case linkage Map.! linkID of
      PlainLink dID ->
        "linkedType" .= showDeclJSON dID
      InheritedLink dID ->   
        "linkedType" .= showDeclJSON dID
      ContractLink cID ->
        "linkedContract" .= contractName cID
      LibraryLink WithSize{stored} ->
        "linkedType" .= showDeclJSON stored <>
        "library" .= declContract stored

showDeclJSON :: DeclID -> String
showDeclJSON DeclID{declContract, declName} = 
  showContractJSON declContract ++ "." ++ declName

showContractJSON :: ContractID -> String
showContractJSON ContractID{contractFile, contractName} =
  "(file " ++ contractFile ++ ")." ++ contractName

