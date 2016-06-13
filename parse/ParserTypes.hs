module ParserTypes where

import Text.Parsec
import Numeric.Natural

type Identifier = String
type ContractName = Identifier
type SourceCode = String
type SolidityParser = Parsec SourceCode ContractName

setContractName :: ContractName -> SolidityParser ()
setContractName = setState

getContractName :: SolidityParser ContractName
getContractName = getState

type SolidityFile = [SolidityContract]

data SolidityContract =
  Contract {
    contractName :: ContractName,
    contractObjs :: [SolidityObjDef],
    contractTypes :: [SolidityTypeDef],
    contractBaseNames :: [(ContractName, SourceCode)]
    }
  deriving (Show)

data SolidityObjDef =
  ObjDef {
    objName :: Identifier,
    objValueType :: SolidityTuple,
    objArgType :: SolidityTuple,
    objDefn :: String
    }
  deriving (Show)
           
data SolidityTypeDef =
  TypeDef {
    typeName :: Identifier,
    typeDecl :: SolidityNewType
    }
  deriving (Show)
           
data SolidityTuple =
  NoValue |
  SingleValue SolidityBasicType |
  TupleValue [SolidityObjDef]
  deriving (Show)

tupleHasValue :: SolidityTuple -> Bool
tupleHasValue NoValue = False
tupleHasValue _ = True

data SolidityBasicType =
  Boolean |
  Address |
  SignedInt   { bytes :: Natural } |
  UnsignedInt { bytes :: Natural } |
  SignedFixed { intBytes :: Natural, fracBytes :: Natural } |
  UnsignedFixed { intBytes :: Natural, fracBytes :: Natural } |
  FixedBytes  { bytes :: Natural } |
  DynamicBytes|
  String |
  FixedArray  { elemType :: SolidityBasicType, fixedLength :: Natural } |
  DynamicArray{ elemType :: SolidityBasicType } |
  Mapping     { domType  :: SolidityBasicType, codType :: SolidityBasicType } |
  Typedef     { typedefName :: Identifier }
  deriving (Show)
  
data SolidityNewType =
  Enum        { names  :: [Identifier] } |
  Struct      { fields :: [SolidityObjDef] } |
  Using       { usingContract :: ContractName, usingType :: Identifier } |
  ContractT
  deriving (Show)
  
type SolidityValue = String
