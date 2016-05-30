module ParserTypes where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Numeric.Natural

type FileName = SourceName
type Identifier = String
type ContractName = Identifier
type SourceCode = String
type SolidityParser = Parsec SourceCode ContractName

setContractName :: ContractName -> SolidityParser ()
setContractName = setState

getContractName :: SolidityParser ContractName
getContractName = getState

data ImportAs =
    Unqualified |
    StarPrefix ContractName |
    Aliases [(ContractName, ContractName)]
    
getImportDefs :: Map FileName (Map ContractName a) -> [(FileName, ImportAs)]
                  -> Map ContractName a
getImportDefs fileDefs imports = Map.unions $ map getQualifiedImports imports
  where
    getQualifiedImports (fileName, importAs) =
      changeNames importAs $
      Map.findWithDefault (error $ "Import not found: " ++ fileName) fileName fileDefs
      where
        changeNames Unqualified = id
        changeNames (StarPrefix p) = Map.mapKeys ((p ++ ".") ++)
        changeNames (Aliases as) = Map.fromList . flip map as . getSym
          where
            getSym m (k, a) =
              (a,
               Map.findWithDefault
                 (error $ "Symbol " ++ k ++ " not found in " ++ fileName)
                 k m
              )

data SolidityFile = 
  SolidityFile {  
    fileContracts :: [SolidityContract],
    fileImports :: [(FileName, ImportAs)]
  }

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
