module ParserTypes where

import Data.Map (Map)
import Data.Traversable
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
   
data ImportError = 
  MissingImport FileName |
  MissingSymbol Identifier FileName |
  MissingBase Identifier

getImportDefs :: Map FileName (Either ImportError (Map ContractName a))
                  -> [(FileName, ImportAs)]
                  -> Either ImportError (Map ContractName a)
getImportDefs fileDefsEither imports = do
  let 
    getQualifiedImports (fileName, importAs) = do
      let
        getFileEither = Map.findWithDefault (Left $ MissingImport fileName) fileName
        getSymbolEither sym = Map.findWithDefault (Left $ MissingSymbol sym fileName) sym
        changeNames = case importAs of
          Unqualified -> sequence
          StarPrefix p -> sequence . Map.mapKeys ((p ++ ".") ++)
          Aliases as -> sequence . Map.fromList . flip map as . getSym
            where getSym m (k, x) = (x, getSymbolEither k m)
      fileDef <- getFileEither fileDefsEither
      let symbolDefsEither = Map.map Right fileDef
      changeNames symbolDefsEither
  imported <- mapM getQualifiedImports imports
  return $ Map.unions imported

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
