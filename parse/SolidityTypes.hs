module SolidityTypes where

import Data.Map (Map)
import Data.Set (Set)

import Text.Parsec (SourceName)
import Numeric.Natural

type FileName = SourceName
type SourceCode = String
type Identifier = String
type ContractName = Identifier
type SolidityFiles = Map FileName SolidityFile
type ContractsByName = Map ContractName SolidityContract

data SolidityFile = 
  SolidityFile {  
    fileContracts :: ContractsByName,
    fileImports :: [(FileName, ImportAs)]
    }

data ImportAs =
    Unqualified |
    StarPrefix ContractName |
    Aliases [(ContractName, ContractName)]
   
data SolidityContract =
  Contract {
    contractVars :: DeclarationsBy SolidityVarDef,
    contractFuncs :: DeclarationsBy SolidityFuncDef,
    contractEvents :: DeclarationsBy SolidityEventDef
    contractModifiers :: DeclarationsBy SolidityModifierDef,
    contractTypes :: DeclarationsBy SolidityTypeDef,
    -- In order of decreasing storage location
    contractStorageVars :: [DeclID],
    -- In order of increasingly derived
    contractInherits :: [ContractName],
    contractExternalNames :: Set ([ContractName], Identifier),
    contractLibraryTypes :: Set DeclID,
    contractIsConcrete :: Bool,
    contractIsLibrary :: Bool
    }

data DeclarationsBy a =
  DeclarationsBy {
    byName :: Map Identifier a,
    byID :: Map DeclID a
    }

data DeclID = 
  DeclID {
    declContract :: ContractName,
    declName :: Identifier,
    } deriving (Eq, Ord)

data SolidityVarDef =
  VarDef {
    varVisibility :: SolidityVisibility,
    varType :: SolidityBasicType,
    varStorage :: SolidityStorage
    }

data SolidityVisibility = PublicVisible | PrivateVisible | InternalVisible | ExternalVisible

data SolidityStorage = StorageStorage | MemoryStorage | IndexedStorage | ValueStorage
           
data SolidityFuncDef =
  FuncDef {
    funcVisibility :: SolidityVisibility,
    funcValueType :: SolidityTuple,
    funcArgType :: SolidityTuple,
    funcHasCode :: Bool,
    funcIsConstant :: Bool
    }

data SolidityEventDef =
  EventDef {
    eventTopics :: SolidityTuple,
    eventIsAnonymous :: Bool
    }

data SolidityModifierDef =
  ModifierDef {
    modDefn :: String,
    modArgs :: SolidityTuple
    }

newtype SolidityTuple = TupleValue [SolidityArgDef]

data SolidityArgDef =
  ArgDef {
    argName :: Identifier,
    argType :: SolidityBasicType,
    argStorage :: SolidityStorage
  }

type SolidityTypeDef = SolidityNewType

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
  Typedef     { typedefPath :: [ContractName], typedefTypeID :: DeclID }
  
data SolidityNewType =
  Enum        { names  :: [Identifier] } |
  Struct      { fields :: [SolidityFieldDef] }

data SolidityFieldDef =
  FieldDef {
    fieldName :: Identifier,
    fieldType :: SolidityBasicType
  }

