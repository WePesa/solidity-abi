-- | 
-- Module: JSON
-- Description: Source for the JSON ABI creator
-- Maintainer: Ryan Reich <ryan@blockapps.net>
{-# LANGUAGE OverloadedStrings, ViewPatterns, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JSON () where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Aeson hiding ((.=), Value(String))
import Data.Aeson.Types (Pair)
import qualified Data.Aeson as Aeson ((.=), Value(String))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Control.Monad.Trans.Reader
import Data.Monoid
import Numeric.Natural

import SolidityTypes
import Selector

newtype AssocJSON = AssocJSON {assocJSON :: [Pair]} deriving (Monoid)

instance ToJSON AssocJSON where
  toJSON = object . assocJSON

class (Monoid kv, ToJSON kv) => KeyValue kv where
  (.=) :: (ToJSON v) => Text -> v -> kv

instance KeyValue AssocJSON where
  (.=) k (toJSON -> Object x) | HashMap.null x = AssocJSON []
  (.=) k (toJSON -> Array x) | Vector.null x = AssocJSON []
  (.=) k (toJSON -> Aeson.String x) | Text.null x = AssocJSON []
  (.=) k (toJSON -> Bool x) | not x = AssocJSON []
  (.=) k (toJSON -> Null) = AssocJSON []
  (.=) k (toJSON -> v) = AssocJSON [k Aeson..= v]

instance ToJSON (Contract 'AfterLayout) where
  toJSON c = toJSON (contractToJSON c :: AssocJSON)
--  toEncoding = pairs . contractToJSON

contractToJSON :: forall kv. (KeyValue kv) => Contract 'AfterLayout -> kv
contractToJSON c = flip runReader typesLinkage $ do
  sJSON :: [kv] <- sequence $
    map (storageVarToJSON . fmap getVarType) storageList
  fJSON :: Map Identifier kv <- sequence $
    Map.mapWithKey funcToJSON (defsByName $ contractFuncs c)
  eJSON :: Map Identifier kv <- sequence $ 
    Map.mapWithKey eventToJSON (defsByName $ contractEvents c)
  tJSON :: Map String kv <- sequence $ 
    Map.map typeToJSON (Map.mapKeys showDeclJSON $ byID $ contractTypes c) 
  cJSON :: Maybe (Map Identifier kv) <- sequence $
    (tupleToJSON . funcArgType) <$> (Map.lookup constrID $ byID $ contractFuncs c)
  return $
    "storage" .= sJSON <>
    "funcs" .= fJSON <>
    "events" .= eJSON <>
    "types" .= tJSON <>
    "constr" .= cJSON <>
    "library" .= contractIsLibrary c <>
    "libraries" .= map contractName librariesLinkage <>
    "vars" .= 
      Map.map (storageMap Map.!) (
        (byName $ contractVars c)
        `Map.intersection`
        (Map.filter isStorageVar $ defsByName $ contractVars c)
        )

  where
    getVarType = varType . (byID (contractVars c) Map.!)
    storageList = reverse $ contractStorageVars c
    storageMap = Map.fromList $ zip (map stored storageList) [0::Integer ..]
    defsByName declsBy = Map.map (byID declsBy Map.!) (byName declsBy)
    CompleteLinkage{typesLinkage, librariesLinkage} = contractLinkage c
    constrID = DeclID{declContract = cID, declName = contractName cID}
    cID = NonEmpty.head $ allBases $ contractBases c

type JSONReader = Reader (LinkageT 'AfterLayout)

funcToJSON :: forall kv. (KeyValue kv) => Identifier -> FuncDef -> JSONReader kv
funcToJSON name FuncDef{funcArgType, funcValueType} = do
  linkage <- ask
  aJSON :: Map Identifier kv <- tupleToJSON funcArgType
  vJSON :: Map Identifier kv <- tupleToJSON funcValueType
  return $
    "selector" .= selector linkage name funcArgType <>
    "args" .= aJSON <>
    "vals" .= vJSON

eventToJSON :: forall kv. (KeyValue kv) => Identifier -> EventDef -> JSONReader kv
eventToJSON name EventDef{eventTopics, eventIsAnonymous} = do
  linkage <- ask
  tJSON :: Map Identifier kv <- tupleToJSON eventTopics
  return $
    "selector" .= 
      (if eventIsAnonymous then Nothing else Just $ selector linkage name eventTopics) <>
    "topics" .= tJSON

typeToJSON :: forall kv. (KeyValue kv) => NewType 'AfterLayout -> JSONReader kv
typeToJSON Enum{names} = return $
  "type" .= ("Enum" :: Text) <>
  "bytes" .= toInteger (sizeOf names) <>
  "names" .= stored names
typeToJSON Struct{fields = WithSize{sizeOf, stored}} = do
  fJSON :: Map Identifier kv <- sequence $ Map.map storageVarToJSON stored
  return $
    "type" .= ("Struct" :: Text) <>
    "bytes" .= toInteger sizeOf <>
    "fields" .= fJSON

tupleToJSON :: forall kv. (KeyValue kv) => Tuple -> JSONReader (Map Identifier kv)
tupleToJSON (TupleValue argsDef) = do
  typesJSON :: [kv] <- mapM makeTypeJSON argsDef
  return $ Map.fromList $ 
    zipWith3 makeIndexAssoc [0::Integer ..] typesJSON $ map argName argsDef 

  where
    makeTypeJSON ArgDef{argType, argStorage} = do
      aJSON :: kv <- basicTypeJSON argType
      return $
        "indexed" .= (argStorage == IndexedStorage) <>
        aJSON
    makeIndexAssoc i aJSON name = (
      if null name then "#" ++ show i else name, 
      "index" .= i <>
      aJSON
      )

storageVarToJSON :: (KeyValue kv) => WithPos BasicType -> JSONReader kv
storageVarToJSON WithPos{startPos, stored} = do
  typeABI :: kv <- basicTypeJSON stored
  return $
    "atBytes" .= toInteger startPos <>
    typeABI

basicTypeJSON :: forall kv. (KeyValue kv) => BasicType -> JSONReader kv
basicTypeJSON t = case t of
  Boolean -> return $
    "type" .= ("Bool" :: Text)
  Address -> return $ 
    "type" .= ("Address" :: Text)
  SignedInt b -> return $ 
    "type" .= ("Int" :: Text) <>
    "signed" .= True <>
    "bytes" .= toInteger b
  UnsignedInt b -> return $
    "type" .= ("Int" :: Text) <>
    "bytes" .= toInteger b
  FixedBytes b -> return $
    "type" .= ("Bytes" :: Text) <>
    "bytes" .= toInteger b
  DynamicBytes -> return $
    "type" .= ("Bytes" :: Text) <>
    "dynamic" .= True
  String -> return $
    "type" .= ("String" :: Text) <>
    "dynamic" .= True
  FixedArray eT l -> do
    eJSON :: kv <- basicTypeJSON eT
    return $ 
      "type" .= ("Array" :: Text) <>
      "length" .= toInteger l <>
      "entry" .= eJSON
  DynamicArray eT -> do
    eJSON :: kv <- basicTypeJSON eT
    return $ 
      "type" .= ("Array" :: Text) <>
      "dynamic" .= True <>
      "entry" .= eJSON
  Mapping dT cT -> do
    kJSON :: kv <- basicTypeJSON dT
    vJSON :: kv <- basicTypeJSON cT
    return $
      "type" .= ("Mapping" :: Text) <>
      "dynamic" .= True <>
      "key" .= kJSON <>
      "value" .= vJSON
  LinkT linkID -> do
    linkage <- ask
    return $ case linkage Map.! linkID of
      PlainLink WithSize{stored} ->
        "linkedType" .= showDeclJSON stored
      InheritedLink WithSize{stored} ->   
        "linkedType" .= showDeclJSON stored
      ContractLink cID ->
        "linkedContract" .= contractName cID
      LibraryLink WithSize{stored} ->
        "linkedType" .= showDeclJSON stored <>
        "library" .= contractName (declContract stored)

showDeclJSON :: DeclID -> String
showDeclJSON DeclID{declContract, declName} = 
  showContractJSON declContract ++ "::" ++ declName

showContractJSON :: ContractID -> String
showContractJSON ContractID{contractFile, contractName} =
  contractFile ++ "::" ++ contractName

