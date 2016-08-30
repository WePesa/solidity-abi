{-# LANGUAGE TypeFamilies, NamedFieldPuns, DataKinds #-}
module SolidityTypes where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List.NonEmpty
import Text.Parsec (SourceName)
import Numeric.Natural

-- The main data type
data Contract (stage :: Stage)
  Contract {
    contractVars :: DeclarationsBy VarDef,
    contractFuncs :: DeclarationsBy FuncDef,
    contractEvents :: DeclarationsBy EventDef,
    contractModifiers :: DeclarationsBy ModifierDef,
    contractTypes :: DeclarationsBy (NewType stage),
    -- In order of decreasing storage location
    contractStorageVars :: StorageVars stage,
    -- In order of increasingly base
    contractBases :: BaseContracts stage,
    contractLinkage :: Linkage stage,
    contractIsConcrete :: Bool,
    contractIsLibrary :: Bool
    }

-- Stages of operation; a data kind
data Stage =
  AfterParsing |
  AfterInheritance |
  AfterLinkage |
  AfterLayout

data RoughStage = Incomplete | Complete

-- Various declaration types
data DeclarationsBy a =
  DeclarationsBy {
    byName :: Map Identifier a,
    byID :: Map DeclID a
    }

data VarDef =
  VarDef {
    varVisibility :: Visibility,
    varType :: BasicType,
    varStorage :: Storage
    }

data FuncDef =
  FuncDef {
    funcVisibility :: Visibility,
    funcValueType :: Tuple,
    funcArgType :: Tuple,
    funcHasCode :: Bool,
    funcIsConstructor :: Bool,
    funcIsConstant :: Bool
    }

data EventDef =
  EventDef {
    eventTopics :: Tuple,
    eventIsAnonymous :: Bool
    }

data ModifierDef =
  ModifierDef {
    modDefn :: String,
    modArgs :: Tuple
    }

data ArgDef =
  ArgDef {
    argName :: Identifier,
    argType :: BasicType,
    argStorage :: Storage
  }

data FieldDef =
  FieldDef {
    fieldName :: Identifier,
    fieldType :: BasicType
  }

newtype Tuple = TupleValue [ArgDef]

data SolidityVisibility =
  PublicVisible | PrivateVisible | InternalVisible | ExternalVisible

data SolidityStorage = 
  StorageStorage | MemoryStorage | IndexedStorage | ValueStorage | TypeStorage
  deriving (Eq, Show)
 
-- Layout, inheritance, and linkage
type family StorageVars (stage :: Stage) where
  StorageVars 'AfterLayout = StorageVarsT 'Complete
  StorageVars _ = StorageVarsT 'Incomplete
type family StorageVarsT (rstage :: RoughStage) where
  StorageVars 'Incomplete = [DeclID]
  StorageVars 'Complete = [WithPos DeclID]
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

type family BaseContracts (stage :: Stage) where
  BaseContracts 'AfterParsing = BaseContractsT 'Incomplete
  BaseContracts _ = BaseContractsT 'Complete
type family BaseContractsT (rstage :: RoughStage) where
  BaseContractsT 'Incomplete = [ContractID]
  BaseContractsT 'Complete = BaseContractsData
data BaseContractsData =
  BaseContracts {
    directBases :: BaseContractsT 'Incomplete,
    allBases :: NonEmpty ContractID
    }

type family Linkage (stage :: Stage) where
  Linkage 'AfterParsing = LinkageT 'Incomplete
  Linkage 'AfterInheritance = LinkageT 'Incomplete
  Linkage _ = 'Complete
type family LinkageT (rstage :: RoughStage) where
  LinkageT 'Incomplete = Map LinkID (LinkT 'Incomplete)
  LinkageT 'Complete = LinkageData
data LinkageData
  CompleteLinkage {
    typedefsLinkage :: Map LinkID (LinkT 'Complete),
    librariesLinkage :: [ContractName]
    }

-- Type-related datatypes
type family NewType (stage :: Stage) where
  NewType 'AfterLayout = NewTypeT 'Complete
  NewType _ = NewTypeT 'Incomplete
data NewTypeT (rstage :: RoughStage) =
  Enum        { names  :: Identifiers rstage } |
  Struct      { fields :: Fields rstage } |
  ContractT   { contractT :: () }
type family Identifiers (rstage :: RoughStage) where
  Identifiers 'Incomplete = [Identifier]
  Identifiers 'Complete = WithPos [Identifier]
type family Fields (rstage :: RoughStage) where
  Fields 'Incomplete = [FieldsDef]
  Fields 'Complete = WithPos [WithPos FieldsDef]

data BasicType = 
  Boolean |
  Address |
  SignedInt   { bytes :: Natural } |
  UnsignedInt { bytes :: Natural } |
  FixedBytes  { bytes :: Natural } |
  DynamicBytes|
  String |
  FixedArray  { elemType :: BasicType, fixedLength :: Natural } |
  DynamicArray{ elemType :: BasicType } |
  Mapping     { domType  :: BasicType, codType :: BasicType } |
  LinkT { linkTo :: LinkID }

data family LinkT (rstage :: RoughStage) 
data instance LinkT 'Incomplete =
  UnqualifiedLink Identifier |
  QualifiedLink {
    linkQualifier :: Identifier,
    linkType :: Identifier
    } deriving (Eq, Ord)
data instance LinkT 'Complete =
  PlainTypedef DeclID |
  ContractTypedef ContractID |
  InheritedTypedef DeclID |
  LibraryTypedef DeclID
 
-- ID types
type FileName = SourceName
type ContractName = Identifier
type Identifier = String
data ContractID = 
  ContractID {
    contractFile :: FileName,
    contractName :: ContractName
    } deriving (Eq, Ord)
data DeclID = 
  DeclID {
    declContract :: ContractID,
    declName :: Identifier
    } deriving (Eq, Ord)
data LinkID = 
  LinkID {
    linkContract :: ContractID,
    linkName :: LinkT 'Incomplete
    } deriving (Eq, Ord)

-- File-level types
data SolidityFile = 
  SolidityFile {  
    fileContracts :: ContractsByName 'AfterParsing,
    fileImports :: [(FileName, ImportAs)]
    }
data ImportAs =
    Unqualified |
    StarPrefix ContractName |
    Aliases [(ContractName, ContractName)]

type SourceCode = String
type ContractsByID (s :: Stage) = Map ContractID (Contract s)
type ContractsByFile (s :: Stage) = Map FileName (ContractsByName s)
type ContractsByName (s :: Stage) = Map ContractName (Contract s)
type SolidityFiles = Map FileName SolidityFile

-- Initial values

emptyContract :: Contract 'AfterParsing
emptyContract = 
  Contract {
    contractStorageVars = [],
    contractVars = emptyDeclsBy,
    contractFuncs = emptyDeclsBy,
    contractEvents = emptyDeclsBy,
    contractModifiers = emptyDeclsBy,
    contractTypes = emptyDeclsBy,
    contractLinkage = Map.empty,
    contractBases = [],
    contractIsConcrete = True,
    contractIsLibrary = False
    }

emptyDeclsBy :: DeclarationsBy a
emptyDeclsBy =
  DeclarationsBy {
    byName = Map.empty,
    byID = Map.empty
    }

-- A convenient accessor
typeSize :: NewType 'Complete -> Natural
typeSize EnumPos{namesPos} = sizeOf namesPos
typeSize StructPos{fieldsPos} = sizeOf fieldsPos
typeSize ContractTPos{contractTPos} = sizeOf contractTPos

