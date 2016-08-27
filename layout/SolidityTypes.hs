{-# LANGUAGE TypeFamilies, EmptyDataDecls, NamedFieldPuns #-}
module SolidityTypes where

import Data.Map (Map)
import qualified Data.Map as Map

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
type SolidityNewTypeABI = SolidityNewTypeT WithPos

data WithoutPos a
data WithPos a =
  WithPos {
    startPos :: Natural,
    endPos :: Natural,
    stored :: a
    } |
  WithSize {
    sizeOf :: Natural,
    stored :: a
    }

type family StorageVars t a where
  StorageVars WithoutPos a = [a]
  StorageVars WithPos a = [WithPos a]

data SolidityContractT (t :: * -> *) =
  Contract {
    contractVars :: DeclarationsBy SolidityVarDef,
    contractFuncs :: DeclarationsBy SolidityFuncDef,
    contractEvents :: DeclarationsBy SolidityEventDef,
    contractModifiers :: DeclarationsBy SolidityModifierDef,
    contractTypes :: DeclarationsBy (SolidityNewTypeT t),
    -- In order of decreasing storage location
    contractStorageVars :: StorageVars t DeclID,
    -- In order of increasingly base
    contractInherits :: [ContractName],
    contractExternalNames :: [([ContractName], Identifier)],
    contractLibraryTypes :: [DeclID],
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
    contractExternalNames = [],
    contractLibraryTypes = [],
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
    declName :: Identifier
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
    funcIsConstructor :: Bool,
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
  
data family SolidityNewTypeT (t :: * -> *)
data instance SolidityNewTypeT WithoutPos =
  Enum        { names  :: [Identifier] } |
  Struct      { fields :: [SolidityFieldDef] } |
  ContractT   { contractT :: () }
data instance SolidityNewTypeT WithPos = 
  EnumPos        { namesPos  :: WithPos [Identifier] } |
  StructPos      { fieldsPos :: WithPos [WithPos SolidityFieldDef] } |
  ContractTPos   { contractTPos :: WithPos () }

typeSize :: SolidityNewTypeT WithPos -> Natural
typeSize EnumPos{namesPos} = sizeOf namesPos
typeSize StructPos{fieldsPos} = sizeOf fieldsPos
typeSize ContractTPos{contractTPos} = sizeOf contractTPos

data SolidityFieldDef =
  FieldDef {
    fieldName :: Identifier,
    fieldType :: SolidityBasicType
  }

