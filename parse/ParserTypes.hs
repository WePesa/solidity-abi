module ParserTypes where

import Text.Parsec
import Numeric.Natural

type SolidityParser = Parsec SourceCode SolidityContract

setContractName :: ContractName -> SolidityParser ()
setContractName n = modifyState $ \c -> c{contractName = n}

addLibraryType :: ContractName -> Identifier -> SolidityParser ()
addLibraryType n t = modifyState $ \c@Contract{contractLibraryTypes = lts} -> c{contractLibraryTypes = (n, t) : lts}

addObj :: SolidityObjDef -> SolidityParser ()
addObj o = modifyState $ \c@Contract{contractObjs = os} -> c{contractObjs = o:os}

addType :: SolidityTypeDef -> SolidityParser ()
addType t = modifyState $ \c@Contract{contractTypes = ts} -> c{contractTypes = t:ts}

addBase :: ContractName -> SourceCode -> SolidityParser ()
addBase n x = modifyState $ \c@Contract{contractBaseNames = bs} -> c{contractBaseNames = (n, x) : bs}

setIsLibrary :: SolidityParser ()
setIsLibrary = modifyState $ \c -> c{contractIsLibrary = True}

emptyContract :: SolidityContract
emptyContract = Contract {
  contractName = "",
  contractObjs = [],
  contractTypes = [],
  contractLibraryTypes = [],
  contractBaseNames = [],
  contractIsLibrary = False
  }

type FileName = SourceName
type Identifier = String
type ContractName = Identifier
type SourceCode = String

data ImportAs =
    Unqualified |
    StarPrefix ContractName |
    Aliases [(ContractName, ContractName)]
    deriving (Eq)
   
data SolidityFile = 
  SolidityFile {  
    fileContracts :: [SolidityContract],
    fileImports :: [(FileName, ImportAs)]
  } deriving (Eq)

data SolidityContract =
  Contract {
    contractName :: ContractName,
    contractObjs :: [SolidityObjDef],
    contractTypes :: [SolidityTypeDef],
    contractLibraryTypes :: [(ContractName, Identifier)],
    contractBaseNames :: [(ContractName, SourceCode)],
    contractIsLibrary :: Bool
    }
  deriving (Eq)

data SolidityObjDef =
  ObjDef {
    objName :: Identifier,
    objValueType :: SolidityTuple,
    objArgType :: SolidityTuple,
    objDefn :: String,
    objVisibility :: SolidityVisibility,
    objStorage :: SolidityStorage
    }
  deriving (Eq)

data SolidityVisibility = PublicVisible | PrivateVisible | InternalVisible | ExternalVisible deriving (Eq)

data SolidityStorage = StorageStorage | MemoryStorage | ConstantStorage deriving (Eq)
           
data SolidityTypeDef =
  TypeDef {
    typeName :: Identifier,
    typeDecl :: SolidityNewType
    }
  deriving (Eq)
           
data SolidityTuple =
  NoValue |
  SingleValue SolidityBasicType |
  TupleValue [SolidityObjDef]
  deriving (Eq)

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
  Typedef     { typedefName :: Identifier, typedefLibrary :: Maybe ContractName }
  deriving (Eq)
  
data SolidityNewType =
  Enum        { names  :: [Identifier] } |
  Struct      { fields :: [SolidityObjDef] } |
  ContractT
  deriving (Eq)
  
