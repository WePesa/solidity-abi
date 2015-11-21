module ParserTypes where

import Control.Arrow
import Data.ByteString (ByteString)
import Data.Functor
import qualified Data.Map.Strict as Map hiding (Map)
import Data.Map.Strict (Map)
import Text.Parsec
import Text.PrettyPrint
import Data.List
import Data.Word

type Identifier = String
type ContractName = Identifier
type SourceCode = String
type SolidityParser = Parsec SourceCode ContractName

setContractName :: ContractName -> SolidityParser ()
setContractName = setState

getContractName :: SolidityParser ContractName
getContractName = getState

data SolidityContract =
  Contract {
    contractName :: ContractName,
    contractObjs :: [SolidityObjDef],
    contractTypes :: [SolidityTypeDef],
    contractBaseNames :: [(ContractName, SourceCode)]
    }

data SolidityObjDef =
  ObjDef {
    objName :: Identifier,
    objValueType :: SolidityTuple,
    objArgType :: SolidityTuple,
    objDefn :: String
    }
data SolidityTypeDef =
  TypeDef {
    typeName :: Identifier,
    typeDecl :: SolidityNewType
    }
data SolidityTuple =
  NoValue |
  SingleValue SolidityBasicType |
  TupleValue [SolidityObjDef]

tupleHasValue :: SolidityTuple -> Bool
tupleHasValue NoValue = False
tupleHasValue _ = True

data SolidityBasicType =
  Boolean |
  Address |
  SignedInt   { bytes :: Integer } |
  UnsignedInt { bytes :: Integer } |
  FixedBytes  { bytes :: Integer } |
  DynamicBytes|
  String |
  FixedArray  { elemType :: SolidityType, fixedLength :: Integer } |
  DynamicArray{ elemType :: SolidityType } |
  Mapping     { domType  :: SolidityType, codType :: SolidityBasicType } |
  TypeDef     { typeName :: Identifier }
data SolidityNewType =
  Enum        { names  :: [Identifier] } |
  Struct      { fields :: [SoliditySymbol] } |
  ContractT

type SolidityValue = String
