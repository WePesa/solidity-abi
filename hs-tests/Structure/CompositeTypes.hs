module Structure.CompositeTypes (test) where

import qualified Data.Map as Map

import Blockchain.Ethereum.Solidity
import Structure.BasicTypes hiding (test)
import Test.Combinators
import Test.Common 
import Structure.Common

test :: TestTree
test = doTests "compositeTypes" structureTest [
  enumVar,
  arrayOfBasic, arrayOfNewType, sizedArray, arithmeticInSizedArray,
  arrayOfArray, sizedArrayOfArray, arrayOfSizedArray, sizedArrayOfSizedArray,
  mappingFromBasicToBasic, 
  mappingFromNewType, mappingFromArray,
  mappingToNewType, mappingToArray, mappingToMapping,
  structOfBasic, structOfComposite
  ]

enumVar :: StructureTestInput
enumVar =
  compositeNewTypeTestInput cName tName id (`enumDefn` names) 
    (typedefType cName tName) (Enum $ WithSize 1 names)
   
  where
    cName = "enumVar"
    tName = "E"
    names = map (:[]) ['A' .. 'Z']

arrayOfBasic :: StructureTestInput
arrayOfBasic = 
  compositeBasicTypeTestInput "arrayOfBasic" (arrayDeclType "uint64" "") 
    (DynamicArray $ UnsignedInt 8)

arrayOfNewType :: StructureTestInput
arrayOfNewType = 
  compositeNewTypeTestInput cName tName (`arrayDeclType` "") (`enumDefn` names)
    (DynamicArray $ typedefType cName tName) (Enum $ WithSize 1 names)
  
  where 
    cName = "arrayOfNewType"
    tName = "E"
    names = map (:[]) ['A' .. 'Z']

sizedArray :: StructureTestInput
sizedArray = 
  compositeBasicTypeTestInput "sizedArray" (arrayDeclType "uint64" "17")
    (FixedArray (UnsignedInt 8) 17)

arithmeticInSizedArray :: StructureTestInput
arithmeticInSizedArray = 
  compositeBasicTypeTestInput "arithmeticInSizedArray"
    (arrayDeclType "uint64" "2**4 + 1") (FixedArray (UnsignedInt 8) 17)

arrayOfArray :: StructureTestInput
arrayOfArray = 
  compositeBasicTypeTestInput "arrayOfArray" 
    (arrayDeclType (arrayDeclType "uint64" "") "")
    (DynamicArray $ DynamicArray $ UnsignedInt 8)

sizedArrayOfArray :: StructureTestInput
sizedArrayOfArray = 
  compositeBasicTypeTestInput "sizedArrayOfArray" 
    (arrayDeclType (arrayDeclType "uint64" "") "17")
    (FixedArray (DynamicArray $ UnsignedInt 8) 17)

arrayOfSizedArray :: StructureTestInput
arrayOfSizedArray = 
  compositeBasicTypeTestInput "arrayOfSizedArray" 
    (arrayDeclType (arrayDeclType "uint64" "17") "")
    (DynamicArray $ FixedArray (UnsignedInt 8) 17)

sizedArrayOfSizedArray :: StructureTestInput
sizedArrayOfSizedArray = 
  compositeBasicTypeTestInput "sizedArrayOfSizedArray" 
    (arrayDeclType (arrayDeclType "uint64" "17") "17")
    (FixedArray (FixedArray (UnsignedInt 8) 17) 17)

mappingFromBasicToBasic :: StructureTestInput
mappingFromBasicToBasic =
  compositeBasicTypeTestInput "mappingFromBasicToBasic" 
    (mappingDeclType "int" "uint")  
    Mapping{domType = SignedInt 32, codType = UnsignedInt 32}

mappingFromNewType :: StructureTestInput
mappingFromNewType =
  compositeNewTypeTestInput cName tName 
    (`mappingDeclType` "uint") (`enumDefn` names)  
    Mapping{domType = typedefType cName tName, codType = UnsignedInt 32}
    (Enum $ WithSize 1 names)

  where
    cName = "mappingFromNewType"
    tName = "E"
    names = map (:[]) ['A' .. 'Z']

mappingFromArray :: StructureTestInput
mappingFromArray = 
  compositeBasicTypeTestInput "mappingFromArrray" 
    (mappingDeclType (arrayDeclType "int" "") "uint")  
    Mapping{domType = DynamicArray (SignedInt 32), codType = UnsignedInt 32}

mappingToNewType :: StructureTestInput
mappingToNewType = 
  compositeNewTypeTestInput cName tName 
    (mappingDeclType "uint") (`enumDefn` names)  
    Mapping{codType = typedefType cName tName, domType = UnsignedInt 32}
    (Enum $ WithSize 1 names)

  where
    cName = "mappingToNewType"
    tName = "E"
    names = map (:[]) ['A' .. 'Z']

mappingToArray :: StructureTestInput
mappingToArray = 
  compositeBasicTypeTestInput "mappingToArrray" 
    (mappingDeclType "uint" (arrayDeclType "int" ""))  
    Mapping{codType = DynamicArray (SignedInt 32), domType = UnsignedInt 32}

mappingToMapping :: StructureTestInput
mappingToMapping =
  compositeBasicTypeTestInput "mappingToArrray" 
    (mappingDeclType "uint" (mappingDeclType "int" "uint"))  
    Mapping{codType = Mapping (SignedInt 32) (UnsignedInt 32), domType = UnsignedInt 32}

structOfBasic :: StructureTestInput
structOfBasic =
  compositeNewTypeTestInput cName tName id (`structDefn` types)
    (typedefType cName tName) (Struct $ WithSize 64 $ typeObjs)

  where
    cName = "structOfBasic"
    tName = "S"
    types = ["int", "uint64", "bool", "byte", "address"]
    typeObjs = Map.fromList $ zipWith makeObjDef [0::Integer ..]
      [
        WithPos 0 31 $ SignedInt 32,
        WithPos 32 39 $ UnsignedInt 8,
        WithPos 40 40 $ Boolean,
        WithPos 41 41 $ FixedBytes 1,
        WithPos 42 61 $ Address
      ]
    makeObjDef n t = ("f" ++ show n, t) 

structOfComposite :: StructureTestInput
structOfComposite =
  compositeNewTypeTestInput cName tName id (`structDefn` types)
    (typedefType cName tName) (Struct $ WithSize 96 typeObjs)

  where
    cName = "structOfComposite"
    tName = "S"
    types = 
      [
        arrayDeclType "int" "", 
        arrayDeclType "uint64" "3", 
        mappingDeclType "bool" "byte"
      ]
    typeObjs = Map.fromList $ zipWith makeObjDef [0::Integer ..]
      [
        WithPos 0 31 $ DynamicArray $ SignedInt 32,
        WithPos 32 63 $ FixedArray (UnsignedInt 8) 3,
        WithPos 64 95 $ Mapping Boolean $ FixedBytes 1
      ]
    makeObjDef n t = ("f" ++ show n, t)

compositeBasicTypeTestInput :: String -> String -> BasicType -> StructureTestInput
compositeBasicTypeTestInput = basicTypeTestInput

compositeNewTypeTestInput :: String -> String -> (String -> String) ->(String -> String) ->
                            BasicType -> NewTypeStructure -> StructureTestInput
compositeNewTypeTestInput cName tName vConstr tConstr v t = (cName, sources, tester) where
  sources = Map.singleton cName source
  source = contractDefn cName $ tConstr tName ## varDecl (vConstr tName) "x"
  tester contracts = 
    varDefnIs cName contracts cName "x" 
      VarDef{
        varVisibility = InternalVisible,
        varStorage = StorageStorage,
        varType = v
        }
    >>
    typeDefnIs cName contracts cName tName t

typedefType :: ContractName -> Identifier -> BasicType
typedefType cName tName = LinkT $ LinkID (ContractID cName cName) $ UnqualifiedLink tName
