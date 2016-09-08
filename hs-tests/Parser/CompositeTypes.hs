module Parser.CompositeTypes (test) where

import Distribution.TestSuite

import Blockchain.Ethereum.Solidity.Parse
import Parser.BasicTypes hiding (test)
import Test.Combinators
import Test.Common ((<>))
import Parser.Common

test :: Test
test = Group "compositeTypes" True $ map parserTest [
  enumVar,
  arrayOfBasic, arrayOfNewType, sizedArray, arithmeticInSizedArray,
  arrayOfArray, sizedArrayOfArray, arrayOfSizedArray, sizedArrayOfSizedArray,
  mappingFromBasicToBasic, 
  mappingFromNewType, mappingFromArray,
  mappingToNewType, mappingToArray, mappingToMapping,
  structOfBasic, structOfComposite
  ]

enumVar :: ParserTestInput
enumVar =
  compositeNewTypeTestInput "enumVar" tName id (flip enumDefn names) 
    (Typedef tName) (Enum names)
   
  where
    tName = "E"
    names = map (:[]) ['A' .. 'Z']

arrayOfBasic :: ParserTestInput
arrayOfBasic = 
  compositeBasicTypeTestInput "arrayOfBasic" (arrayDeclType "uint64" "") 
    (DynamicArray $ UnsignedInt 8)

arrayOfNewType :: ParserTestInput
arrayOfNewType = 
  compositeNewTypeTestInput "arrayOfNewType" tName
    (flip arrayDeclType "") (flip enumDefn names) (DynamicArray $ Typedef tName) (Enum names)
  
  where 
    tName = "E"
    names = map (:[]) ['A' .. 'Z']

sizedArray :: ParserTestInput
sizedArray = 
  compositeBasicTypeTestInput "sizedArray" (arrayDeclType "uint64" "17")
    (FixedArray (UnsignedInt 8) 17)

arithmeticInSizedArray :: ParserTestInput
arithmeticInSizedArray = 
  compositeBasicTypeTestInput "arithmeticInSizedArray"
    (arrayDeclType "uint64" "2**4 + 1") (FixedArray (UnsignedInt 8) 17)

arrayOfArray :: ParserTestInput
arrayOfArray = 
  compositeBasicTypeTestInput "arrayOfArray" 
    (arrayDeclType (arrayDeclType "uint64" "") "")
    (DynamicArray $ DynamicArray $ UnsignedInt 8)

sizedArrayOfArray :: ParserTestInput
sizedArrayOfArray = 
  compositeBasicTypeTestInput "sizedArrayOfArray" 
    (arrayDeclType (arrayDeclType "uint64" "") "17")
    (FixedArray (DynamicArray $ UnsignedInt 8) 17)

arrayOfSizedArray :: ParserTestInput
arrayOfSizedArray = 
  compositeBasicTypeTestInput "arrayOfSizedArray" 
    (arrayDeclType (arrayDeclType "uint64" "17") "")
    (DynamicArray $ FixedArray (UnsignedInt 8) 17)

sizedArrayOfSizedArray :: ParserTestInput
sizedArrayOfSizedArray = 
  compositeBasicTypeTestInput "sizedArrayOfSizedArray" 
    (arrayDeclType (arrayDeclType "uint64" "17") "17")
    (FixedArray (FixedArray (UnsignedInt 8) 17) 17)

mappingFromBasicToBasic :: ParserTestInput
mappingFromBasicToBasic =
  compositeBasicTypeTestInput "mappingFromBasicToBasic" 
    (mappingDeclType "int" "uint")  
    Mapping{domType = SignedInt 32, codType = UnsignedInt 32}

mappingFromNewType :: ParserTestInput
mappingFromNewType =
  compositeNewTypeTestInput "mappingFromNewType" tName 
    (flip mappingDeclType "uint") (flip enumDefn names)  
    Mapping{domType = Typedef tName, codType = UnsignedInt 32}
    Enum{names}

  where
    tName = "E"
    names = map (:[]) ['A' .. 'Z']

mappingFromArray :: ParserTestInput
mappingFromArray = 
  compositeBasicTypeTestInput "mappingFromArrray" 
    (mappingDeclType (arrayDeclType "int" "") "uint")  
    Mapping{domType = DynamicArray (SignedInt 32), codType = UnsignedInt 32}

mappingToNewType :: ParserTestInput
mappingToNewType = 
  compositeNewTypeTestInput "mappingToNewType" tName 
    (mappingDeclType "uint") (flip enumDefn names)  
    Mapping{codType = Typedef tName, domType = UnsignedInt 32}
    Enum{names}

  where
    tName = "E"
    names = map (:[]) ['A' .. 'Z']

mappingToArray :: ParserTestInput
mappingToArray = 
  compositeBasicTypeTestInput "mappingToArrray" 
    (mappingDeclType "uint" (arrayDeclType "int" ""))  
    Mapping{codType = DynamicArray (SignedInt 32), domType = UnsignedInt 32}

mappingToMapping :: ParserTestInput
mappingToMapping =
  compositeBasicTypeTestInput "mappingToArrray" 
    (mappingDeclType "uint" (mappingDeclType "int" "uint"))  
    Mapping{codType = Mapping (SignedInt 32) (UnsignedInt 32), domType = UnsignedInt 32}

structOfBasic :: ParserTestInput
structOfBasic =
  compositeNewTypeTestInput "structOfBasic" tName id (flip structDefn types)
    (Typedef tName) (Struct typeObjs)

  where
    tName = "S"
    types = ["int", "uint64", "bool", "byte", "address"]
    typeObjs = zipWith makeObjDef [0::Integer ..]
      [
        SignedInt 32,
        UnsignedInt 8,
        Boolean,
        FixedBytes 1,
        Address
      ]
    makeObjDef n t = 
      ObjDef {
        objName = "f" ++ show n,
        objValueType = SingleValue t,
        objArgType = NoValue,
        objDefn = ""
      }

structOfComposite :: ParserTestInput
structOfComposite =
  compositeNewTypeTestInput "structOfComposite" tName id (flip structDefn types)
    (Typedef tName) (Struct typeObjs)

  where
    tName = "S"
    types = 
      [
        arrayDeclType "int" "", 
        arrayDeclType "uint64" "3", 
        mappingDeclType "bool" "byte"
      ]
    typeObjs = zipWith makeObjDef [0::Integer ..]
      [
        DynamicArray $ SignedInt 32,
        FixedArray (UnsignedInt 8) 3,
        Mapping Boolean $ FixedBytes 1
      ]
    makeObjDef n t = 
      ObjDef {
        objName = "f" ++ show n,
        objValueType = SingleValue t,
        objArgType = NoValue,
        objDefn = ""
      }

compositeBasicTypeTestInput :: String -> String -> SolidityBasicType -> ParserTestInput
compositeBasicTypeTestInput = basicTypeTestInput

compositeNewTypeTestInput :: String -> String -> (String -> String) ->(String -> String) ->
                             SolidityBasicType -> SolidityNewType -> ParserTestInput
compositeNewTypeTestInput cName tName vConstr tConstr v t = (cName, source, tester) where
  source = contractDefn cName $ tConstr tName ## varDecl (vConstr tName) "x"
  tester = \solFile ->
    varTypeIs cName solFile cName "x" v <>
    typeDefnIs cName solFile cName tName t
