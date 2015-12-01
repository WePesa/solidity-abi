{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JSON (jsonABI) where

import Data.Aeson hiding (String)
import qualified Data.Aeson as Aeson (Value(String))
import Data.Aeson.Types (Pair)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Data.String
import Data.Text (Text)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Text as Text

import Layout
import ParserTypes
import Selector

instance ToJSON SolidityFile where
  toJSON = jsonABI

jsonABI :: SolidityFile -> Value
jsonABI f = object $ map (contractABI $ layout f) f

contractABI :: SolidityFileLayout -> SolidityContract -> Pair
contractABI fL (Contract name objs types baseNames) =
  pair name $ object $
      (nonempty (pair "vars") $ varsABI (objsLayout $ fL Map.! name) objs) ++
      (nonempty (pair "funcs") $ funcsABI objs) ++
      (nonempty (pair "types") $ typesABI (typesLayout $ fL Map.! name) types) ++
      (nonempty (pair "bases") $ toJSON $ map fst baseNames) ++
      (nonempty (pair "constr") $ constrABI name objs)
  where
    nonempty :: (Value -> Pair) -> Value -> [Pair]
    nonempty f ob@(Object o) =
      if HashMap.null o
      then []
      else [f ob]
    nonempty f ar@(Array a) =
      if Vector.null a
      then []
      else [f ar]
    nonempty f st@(Aeson.String s) =
      if Text.null s
      then []
      else [f st]
    nonempty f Null = []
    nonempty f x = [f x]

varsABI :: SolidityObjsLayout -> [SolidityObjDef] -> Value
varsABI layout objs = object $ catMaybes $ map (\o -> varABI layout o) objs

funcsABI :: [SolidityObjDef] -> Value
funcsABI objs = object $ catMaybes $ map funcABI objs
              
typesABI :: SolidityTypesLayout -> [SolidityTypeDef] -> Value
typesABI layout types = object $ catMaybes $
                        map (\t -> typeABI (layout Map.! typeName t) t) types

constrABI :: Identifier -> [SolidityObjDef] -> Value
constrABI name objs = object $ maybe [] listABI argsM
  where
    argsM = getArgs =<< List.find isConstr objs
    isConstr (ObjDef name' (SingleValue (Typedef name'')) (TupleValue _) _)
      | name == name' && name == name'' = True
    isConstr _ = False
    getArgs (ObjDef _ _ (TupleValue args) _) = Just args
    getArgs _ = Nothing

listABI :: [SolidityObjDef] -> [Pair]
listABI objs = do
  (i, (oName, oABI)) <- zip [1::Integer ..] $ fromMaybe [] $ mapM objABI objs
  let realName = if null oName then "#" ++ show i else oName
  return $ pair realName $ object $ (pair "index" i) : oABI

varABI :: SolidityObjsLayout -> SolidityObjDef -> Maybe Pair
varABI layout obj = do
  (name, tABI) <- objABI obj
  let oB = objStartBytes $ layout Map.! objName obj
  return $ pair name $ object $ pair "atBytes" (toInteger oB) : tABI

funcABI :: SolidityObjDef -> Maybe Pair
funcABI (ObjDef name (TupleValue vals) (TupleValue args) _) =
  Just $ pair name $ object [
           pair "selector" $ selector name args vals,
           lpair "args" args,
           lpair "vals" vals
           ]
funcABI _ = Nothing

typeABI :: SolidityTypeLayout -> SolidityTypeDef -> Maybe Pair
typeABI (StructLayout fieldsL _) (TypeDef name (Struct fields)) =
  Just $ pair name $ object [
    pair "type" "Struct",
    pair "fields" $ varsABI fieldsL fields
    ]
typeABI (EnumLayout tB) (TypeDef name (Enum names)) =
  Just $ pair name $ object $ [
    pair "type" "Enum",
    pair "bytes" $ toInteger tB,
    pair "names" names
    ]
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
  pair "typedef" name
  ]

pair :: (ToJSON a) => String -> a -> Pair
pair x y = (fromString x, toJSON y)

tpair :: String -> SolidityBasicType -> Pair
tpair x y = (fromString x, object $ basicTypeABI y)

lpair :: String -> [SolidityObjDef] -> Pair
lpair x y = (fromString x, object $ listABI y)
