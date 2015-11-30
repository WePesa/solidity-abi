{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JSON (jsonABI) where

import Data.Aeson hiding (String)
import Data.Aeson.Types (Pair)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Text (Text)

import Layout
import ParserTypes
import Selector

instance ToJSON SolidityFile where
  toJSON = jsonABI

jsonABI :: SolidityFile -> Value
jsonABI f = object $ map (contractABI $ layout f) f

contractABI :: SolidityFileLayout -> SolidityContract -> Pair
contractABI fL (Contract name objs types baseNames) =
  pair name $ object [
      pair "vars" $ varsABI (objsLayout $ fL Map.! name) objs,
      pair "funcs" $ funcsABI objs,
      pair "types" $ typesABI (typesLayout $ fL Map.! name) types,
      pair "bases" $ toJSON $ map fst baseNames
      ]

varsABI :: SolidityObjsLayout -> [SolidityObjDef] -> Value
varsABI layout objs = object $ catMaybes $
                      map (\o -> varABI (layout Map.! objName o) o) objs

funcsABI :: [SolidityObjDef] -> Value
funcsABI objs = object $ catMaybes $ map funcABI objs
              
typesABI :: SolidityTypesLayout -> [SolidityTypeDef] -> Value
typesABI layout types = object $ catMaybes $
                        map (\t -> typeABI (layout Map.! typeName t) t) types

listABI :: [SolidityObjDef] -> [Pair]
listABI objs = map (uncurry pair) $ catMaybes $ map objABI objs

varABI :: SolidityObjLayout -> SolidityObjDef -> Maybe Pair
varABI (ObjLayout oB _) obj = do
  (name, tABI) <- objABI obj
  return $ pair name $ object $ pair "atBytes" (toInteger oB) : tABI

funcABI :: SolidityObjDef -> Maybe Pair
funcABI f@(ObjDef name (TupleValue vals) (TupleValue args) _) =
  Just $ pair name $ object [
           pair "selector" $ selector name args vals,
           lpair "args" args,
           lpair "vals" vals
           ]
funcABI _ = Nothing

typeABI :: SolidityTypeLayout -> SolidityTypeDef -> Maybe Pair
typeABI (StructLayout fieldsL _) (TypeDef name (Struct fields)) =
  Just $ pair name $ varsABI fieldsL fields
typeABI (EnumLayout tB) (TypeDef name (Enum names)) =
  Just $ pair name $ object $ [pair "bytes" $ toInteger tB, pair "names" names]
typeABI _ _ = Nothing

objABI :: SolidityObjDef -> Maybe (String, [Pair])
objABI (ObjDef name (SingleValue t) NoValue _) =
  Just (name, basicTypeABI t)
objABI _ = Nothing

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
basicTypeABI (Typedef name) = [
  pair "type" name,
  pair "typeDef" True
  ]

pair :: (ToJSON a) => String -> a -> Pair
pair x y = (fromString x, toJSON y)

tpair :: String -> SolidityBasicType -> Pair
tpair x y = (fromString x, object $ basicTypeABI y)

lpair :: String -> [SolidityObjDef] -> Pair
lpair x y = (fromString x, object $ listABI y)
