module JSON (jsonABI) where

import Data.Aeson
import Data.Maybe

import Layout
import ParserTypes
import Selector

instance ToJSON SolidityFile where
  toJSON = jsonABI

jsonABI :: SolidityFile -> Value
jsonABI f = object $ map contractABI (layout f) f

contractABI :: SolidityFileLayout -> SolidityContract -> [Pair]
contractABI fL (Contract name objs types baseNames) =
  (name, object [
      ("vars", varsABI (objsLayout $ fL ! name) objs),
      ("funcs", funcsABI objs),
      ("types", typesABI (typesLayout $ fL ! name) types),
      ("bases", toJSON $ map fst baseNames)
      ]
  )

varsABI :: SolidityObjsLayout -> [SolidityObjDef] -> Value
varsABI layout objs = object $ catMaybes $ map (\o -> varABI (layout ! o) o) objs

funcsABI :: [SolidityObjDef] -> Value
funcsABI objs = object $ catMaybes $ map funcABI objs
              
typesABI :: SolidityTypesLayout -> [SolidityTypeDef] -> Value
typesABI layout types = object $ catMaybes $ map (\t -> typeABI (layout ! t) t) types

listABI :: [SolidityObjDef] -> [Pair]
listABI objs = catMaybes $ map objABI objs

varABI :: SolidityObjLayout -> SolidityObjDef -> Maybe Pair
varABI (ObjLayout oB _) obj = do
  (name, tABI) <- objABI obj
  return (name, object $ pair "atBytes" oB : tABI)

funcABI :: SolidityObjDef -> Maybe Pair
funcABI f(ObjDef name (TupleValue vals) (TupleValue args) _) =
  Just (name, object $ [
           pair "selector" $ selector name args vals,
           lpair "args" args,
           lpair "vals" vals
           ]
       )
funcABI _ = Nothing

typeABI :: SolidityTypeLayout -> SolidityTypeDef -> Maybe Pair
typeABI (StructLayout fieldsL _) (TypeDef name (Struct fields)) =
  Just (name, varsABI fieldsL fields)
typeABI (EnumLayout tB) (TypeDef name (Enum names)) =
  Just (name, object $ [pair "bytes" tB, pair "names" names])
typeABI _ = Nothing

objABI :: SolidityObjDef -> Maybe (String, [Pair])
objABI (ObjDef name (SingleValue t) NoValue _) = Just (name, basicTypeABI t)
objABI _ = Nothing

basicTypeABI :: SolidityBasicType -> [Pair]
basicTypeABI Boolean = [pair "type" "Bool"]
basicTypeABI Address = [pair "type" "Address"]
basicTypeABI SignedInt b = [
  pair "type" "Int",
  pair "signed" True,
  pair "bytes" b]
basicTypeABI UnsignedInt b = [
  pair "type" "Int",
  pair "bytes" b
  ]
basicTypeABI FixedBytes b = [
  pair "type" "Bytes",
  pair "bytes" b
  ]
basicTypeABI DynamicBytes = [
  pair "type" "Bytes",
  pair "dynamic" True
  ]
basicTypeABI String = [
  pair "type" "String",
  pair "dynamic" True
  ]
basicTypeABI FixedArray eT l = [
  pair "type" "Array",
  pair "length" l,
  tpair "entry" eT
  ]
basicTypeABI DynamicArray eT = [
  pair "type" "Array",
  pair "dynamic" True,
  tpair "entry" eT
  ]
basicTypeABI Mapping dT cT = [
  pair "type" "Mapping",
  pair "dynamic" True,
  tpair "key" dT,
  tpair "value" cT
  ]
basicTypeABI TypeDef name = [
  pair "type" name
  pair "typeDef" True
  ]

pair :: (ToJSON a) => String -> a -> Pair
pair x y = (x, toJSON y)

tpair :: String -> SolidityBasicType -> Pair
tpair x y = (x, object $ basicTypeABI t)

lpair :: String -> [SolidityObjDef] -> Pair
lpair x y = (x, object $ listABI y)
