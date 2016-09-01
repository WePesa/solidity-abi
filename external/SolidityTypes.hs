{-# LANGUAGE UndecidableInstances, TypeFamilies, DeriveFunctor #-}
module SolidityTypes where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import Text.Parsec (SourceName)
import Numeric.Natural

-- The main data type
data Contract (stage :: Stage) =
  Contract {
    -- These fields depend on the stage.
    contractTypes :: DeclarationsBy (NewType stage),
    contractStorageVars :: StorageVars stage, -- In order of decreasing storage location
    contractBases :: BaseContracts stage, -- In order of increasingly base
    contractLinkage :: Linkage stage,

    -- These fields don't depend on the stage
    contractVars :: DeclarationsBy VarDef,
    contractFuncs :: DeclarationsBy FuncDef,
    contractEvents :: DeclarationsBy EventDef,
    contractModifiers :: DeclarationsBy ModifierDef,
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
    byName :: Map Identifier DeclID,
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

data Visibility =
  PublicVisible | PrivateVisible | InternalVisible | ExternalVisible

data Storage = 
  StorageStorage | MemoryStorage | IndexedStorage | ValueStorage | TypeStorage
  deriving (Eq, Show)
 
-- Layout, inheritance, and linkage
type family StorageVars (stage :: Stage) where
  StorageVars 'AfterLayout = StorageVarsT 'Complete 
  StorageVars s = StorageVarsT 'Incomplete 
type family StorageVarsT (rstage :: RoughStage) where
  StorageVarsT 'Incomplete = [DeclID]
  StorageVarsT 'Complete = [WithPos DeclID]
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
  deriving (Functor)

type family BaseContracts (stage :: Stage) where
  BaseContracts 'AfterParsing = BaseContractsT 'Incomplete
  BaseContracts s = BaseContractsT 'Complete
type family BaseContractsT (rstage :: RoughStage) where
  BaseContractsT 'Incomplete = [ContractID]
  BaseContractsT 'Complete = BaseContractsData
data BaseContractsData =
  BaseContracts {
    directBases :: BaseContractsT 'Incomplete,
    allBases :: NonEmpty ContractID
    }

type family Linkage (stage :: Stage) where
  Linkage 'AfterLinkage = LinkageData 'AfterLinkage
  Linkage 'AfterLayout = LinkageData 'AfterLayout
  Linkage s = LinkageT s
type LinkageT (s :: Stage) = Map LinkID (TypeLink s)
data LinkageData (s :: Stage) =
  CompleteLinkage {
    typesLinkage :: LinkageT s,
    librariesLinkage :: [ContractID]
    }

-- Type-related datatypes
type family NewType (stage :: Stage) where
  NewType 'AfterLayout = NewTypeT 'Complete
  NewType s = NewTypeT 'Incomplete
data NewTypeT (rstage :: RoughStage) =
  Enum        { names  :: Identifiers rstage } |
  Struct      { fields :: Fields rstage }
type family Identifiers (rstage :: RoughStage) where
  Identifiers 'Incomplete = [Identifier]
  Identifiers 'Complete = WithPos [Identifier]
type family Fields (rstage :: RoughStage) where
  Fields 'Incomplete = [FieldDef]
  Fields 'Complete = WithPos (Map Identifier (WithPos BasicType))

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

type family TypeLink (stage :: Stage) where
  TypeLink 'AfterLayout = DetailedLink 'Complete
  TypeLink 'AfterLinkage = DetailedLink 'Incomplete
  TypeLink s = RoughLink
data RoughLink =
  UnqualifiedLink Identifier |
  QualifiedLink {
    linkQualifier :: Identifier,
    linkName :: Identifier
    } deriving (Eq, Ord)
data DetailedLink (rstage :: RoughStage) =
  PlainLink (DeclLink rstage) |
  InheritedLink (DeclLink rstage) |
  LibraryLink (DeclLink rstage) |
  ContractLink ContractID
type family DeclLink (rstage :: RoughStage) where
  DeclLink 'Incomplete = DeclID
  DeclLink 'Complete = WithPos DeclID
 
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
    linkIs :: RoughLink
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
    contractTypes = emptyDeclsBy,
    contractStorageVars = [],
    contractBases = [],
    contractLinkage = Map.empty,

    contractVars = emptyDeclsBy,
    contractFuncs = emptyDeclsBy,
    contractEvents = emptyDeclsBy,
    contractModifiers = emptyDeclsBy,
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
typeSize :: NewType 'AfterLayout -> Natural
typeSize Enum{names} = sizeOf names
typeSize Struct{fields} = sizeOf fields

isStorageVar :: VarDef -> Bool
isStorageVar VarDef{varStorage = StorageStorage} = True
isStorageVar _ = False

