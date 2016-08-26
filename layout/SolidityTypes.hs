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
type ContractsABIByName = Map ContractName SolidityContractABI

data SolidityFile = 
  SolidityFile {  
    fileContracts :: ContractsByName,
    fileImports :: [(FileName, ImportAs)]
    }

data ImportAs =
    Unqualified |
    StarPrefix ContractName |
    Aliases [(ContractName, ContractName)]
   
type SolidityContract = SolidityContractT WithoutPos
type SolidityContractABI = SolidityContractT WithPos
type SolidityNewType = SolidityNewTypeT WithoutPos
type SolidityNewTypeABI = SolidityNewType WithPos

type WithoutPos a = a
data WithPos a =
  WithPos {
    startPos :: Natural,
    endPos :: Natural,
    stored :: a
    }

data SolidityContractT t =
  Contract {
    contractVars :: DeclarationsBy SolidityVarDef,
    contractFuncs :: DeclarationsBy SolidityFuncDef,
    contractEvents :: DeclarationsBy SolidityEventDef
    contractModifiers :: DeclarationsBy SolidityModifierDef,
    contractTypes :: DeclarationsBy (SolidityNewTypeT t),
    -- In order of decreasing storage location
    contractStorageVars :: [t DeclID],
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

emptyContract :: SolidityContract
emptyContract = 
  Contract {
    contractStorageVars = [],
    contractVars = emptyDeclsBy,
    contractFuncs = emptyDeclsBy,
    contractEvents = emptyDeclsBy,
    contractModifiers = emptyDeclsBy,
    contractTypes = emptyDeclsBy,
    contractExternalNames = Set.empty,
    contractLibraryTypes = Set.empty,
    contractInherits = [],
    contractIsConcrete = True,
    contractIsLibrary = False
    }

emptyDeclsBy :: DeclarationsBy a
emptyDeclsBy =
  DeclarationsBy {
    byName = Map.empty,
    byID = Map.empty
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
  Typedef     { typedefTypeID :: DeclID }
  
data SolidityNewTypeT t =
  Enum        { names  :: t [Identifier] } |
  Struct      { fields :: t [t SolidityFieldDef] } |
  ContractT   { contractT :: t () }

data SolidityFieldDef =
  FieldDef {
    fieldName :: Identifier,
    fieldType :: SolidityBasicType
  }

