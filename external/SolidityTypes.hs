-- | 
-- Module: SolidityTypes
-- Description: Types used throughout solidity-abi, primarily the ones
--   containing the structure of a parsed contract.  
-- Maintainer: Ryan Reich <ryan@blockapps.net>
{-# LANGUAGE UndecidableInstances, TypeFamilies, DeriveFunctor, StandaloneDeriving, FlexibleInstances #-}
module SolidityTypes where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List.NonEmpty (NonEmpty)

import Text.Parsec (SourceName)
import Numeric.Natural

-- | The structure of a parsed contract.
-- The Stage business unifies the changing formats of this structure as the
-- parsing and anaysis continues.  
type ContractStructure = Contract 'AfterLayout
data Contract (stage :: Stage) =
  Contract {
    -- These fields depend on the stage.
    -- | Types defined in the contract
    contractTypes :: DeclarationsBy (NewType stage),
    -- | Storage variables.  Unlike generic variables, these have to be
    -- stored sequentially (in order of decreasing storage location)
    -- because the order of declaration matters.
    contractStorageVars :: StorageVars stage,
    -- | Base contracts for inheritance.  These also have to be stored
    -- sequentially, in order of decreasing derivedness, for the sake of
    -- the C3 algorithm
    contractBases :: BaseContracts stage,
    -- | References to other contracts, which can occur in various ways.
    contractLinkage :: Linkage stage,

    -- These fields don't depend on the stage
    -- | The original name of the contract, before import aliasing
    contractRealName :: ContractName,
    -- | All variables, storage or not.
    contractVars :: DeclarationsBy VarDef,
    -- | All functions
    contractFuncs :: DeclarationsBy FuncDef,
    -- | All events
    contractEvents :: DeclarationsBy EventDef,
    -- | All modifiers
    contractModifiers :: DeclarationsBy ModifierDef,
    -- | Whether the contract is "concrete", i.e. has no unimplemented
    -- functions.
    contractIsConcrete :: Bool,
    -- | Whether the contract was declared as a library.
    contractIsLibrary :: Bool
    }

-- | Stages of operation; a data kind
data Stage =
  AfterParsing |
  AfterInheritance |
  AfterLinkage |
  AfterLayout

-- | For condensing stage definitions when a type just flips at a single
-- stage.
data RoughStage = Incomplete | Complete

-- Various declaration types
-- | Holds two different indices of declarations: one mapping names to ID
-- (when imports and inheritance are resolved, it is not a priori knowable
-- from the name what the full ID is, without carrying this info); and the
-- other mapping ID to actual declaration.
data DeclarationsBy a =
  DeclarationsBy {
    byName :: Map Identifier DeclID,
    byID :: Map DeclID a
    }
byNameDirectly :: DeclarationsBy a -> Map Identifier a
byNameDirectly declsBy = Map.map (byID declsBy Map.!) $ byName declsBy

-- | All metadata for a variable definition.  This is effectively just
-- a type with a few modifiers as to when it shows up.
data VarDef =
  VarDef {
    varVisibility :: Visibility,
    varType :: BasicType,
    varStorage :: Storage
    }
  deriving (Eq, Show)

-- | All metadata for a function definition.  We don't store the body since
-- it doesn't affect the externally visible contract structure, but we do
-- remember whether it was given, because that affects whether the contract
-- is concrete.
data FuncDef =
  FuncDef {
    funcVisibility :: Visibility,
    funcValueType :: Tuple,
    funcArgType :: Tuple,
    funcHasCode :: Bool,
    funcIsConstructor :: Bool,
    funcIsConstant :: Bool
    }
  deriving (Eq, Show)

-- | All metadata for an event definition.  Events have almost no
-- structure.
data EventDef =
  EventDef {
    eventTopics :: Tuple,
    eventIsAnonymous :: Bool
    }
  deriving (Eq, Show)

-- | All metadata for a modifier definition.  We hardly do anything with
-- modifiers, so we just store the arguments and body without any
-- interpretation.
data ModifierDef =
  ModifierDef {
    modDefn :: String,
    modArgs :: Tuple
    }
  deriving (Eq, Show)

-- | A variant of VarDef for a function argument or co-argument (i.e.
-- return value).  The difference is that while variables are generally
-- stored by name or ID in a map, arguments are stored in sequential lists
-- so have to include their names.  They also have slightly different
-- metadata.
data ArgDef =
  ArgDef {
    argName :: Identifier,
    argType :: BasicType,
    argStorage :: Storage
  }
  deriving (Eq, Show)

-- | Like ArgDef, but for struct fields.
data FieldDef =
  FieldDef {
    fieldName :: Identifier,
    fieldType :: BasicType
  }
  deriving (Eq, Show)

-- | Solidity doesn't have tuple types, but it does have comma-separated
-- lists of function arguments and return values.  Also, although we don't
-- compile executable code, it has structured assignments via tuples.
newtype Tuple = TupleValue [ArgDef] deriving (Eq, Show)

-- | The different contexts in which a function or variable may be exposed.
-- This affects inheritance and also the final ABI.
data Visibility =
  PublicVisible | PrivateVisible | InternalVisible | ExternalVisible
  deriving (Eq, Show)

-- | The different ways a value can be stored
--  - Storage: in Ethereum storage, a persistent database
--  - Memory: in EVM runtime memory.  What that means for us is that it is
--  not in storage, i.e. not in contractStorageVars.
--  - Indexed: for event topics
--  - Value: for constants
--  - Type: a storage concept internal to this parser; the default storage
--  for struct fields.
data Storage = 
  StorageStorage | MemoryStorage | IndexedStorage | ValueStorage | TypeStorage
  deriving (Eq, Show)
 
-- Layout, inheritance, and linkage
-- | Storage variable container type by stage
type StorageVarsStructure = StorageVars 'AfterLayout
type family StorageVars (stage :: Stage) where
  StorageVars 'AfterLayout = StorageVarsT 'Complete 
  StorageVars s = StorageVarsT 'Incomplete 
-- | ...by rough stage.  When complete, we have determined the exact
-- storage locations of the variables.
type family StorageVarsT (rstage :: RoughStage) where
  StorageVarsT 'Incomplete = [DeclID]
  StorageVarsT 'Complete = [WithPos DeclID]
-- | Annotates a type with a storage location
data WithPos a =
  -- | When we know the exact location of the variable.
  WithPos {
    startPos :: Natural,
    endPos :: Natural,
    stored :: a
    } |
  -- | For things that are not exactly stored, but which take up storage.
  WithSize {
    sizeOf :: Natural,
    stored :: a
    }
  deriving (Eq, Show, Functor)

-- | Inheritance base contracts container by stage
type BaseContractsStructure = BaseContracts 'AfterLayout
type family BaseContracts (stage :: Stage) where
  BaseContracts 'AfterParsing = BaseContractsT 'Incomplete
  BaseContracts s = BaseContractsT 'Complete
-- | ...by rough stage.
--  - 'Incomplete: We have only the direct bases in the order they were
--  given.
--  - 'Complete: We have fully resolved inheritance and thus know all the
--  bases, still in order, with the direct ones singled out in case of
--  further inheritance at a later time.
type family BaseContractsT (rstage :: RoughStage) where
  BaseContractsT 'Incomplete = [ContractID]
  BaseContractsT 'Complete = BaseContractsData
-- | Description of the complete information on contract inheritance
data BaseContractsData =
  BaseContracts {
    directBases :: BaseContractsT 'Incomplete,
    allBases :: NonEmpty ContractID
    }

-- | Linkage container by stage
type LinkageStructure = Linkage 'AfterLayout
type family Linkage (stage :: Stage) where
  Linkage 'AfterLinkage = LinkageData 'AfterLinkage
  Linkage 'AfterLayout = LinkageData 'AfterLayout
  Linkage s = LinkageT s
-- | ...by slightly rougher stage
type LinkageT (s :: Stage) = Map LinkID (TypeLink s)
-- | Container of all externally located types used in the contract.
data LinkageData (s :: Stage) =
  CompleteLinkage {
    -- | The types themselves, by ID
    typesLinkage :: LinkageT s,
    -- | The libraries they may have come from.
    librariesLinkage :: [ContractID]
    }

-- Type-related datatypes
-- | Types that can be defined by the programmer, by stage
type NewTypeStructure = NewType 'AfterLayout
type family NewType (stage :: Stage) where
  NewType 'AfterLayout = NewTypeT 'Complete
  NewType s = NewTypeT 'Incomplete
-- | ...by rough stage.  The only user-defined types are enums and structs.
data NewTypeT (rstage :: RoughStage) =
  -- | Order of names is important, because they are numbered consecutively
  -- from 0.
  Enum        { names  :: Identifiers rstage } |
  -- | Structs are formatted very similarly to contracts.  Their objects
  -- can only be of "variable" kind, however.
  Struct      { fields :: Fields rstage }
deriving instance Show (NewTypeT 'Complete)
deriving instance Eq (NewTypeT 'Complete)
-- | Just a list of enum names, but when complete, it also includes its own
-- storage size.
type family Identifiers (rstage :: RoughStage) where
  Identifiers 'Incomplete = [Identifier]
  Identifiers 'Complete = WithPos [Identifier]
-- | When incomplete, we need the fields in order so we can lay them out in
-- storage.  When complete, we can store them by name together with their
-- storage locations.
type family Fields (rstage :: RoughStage) where
  Fields 'Incomplete = [FieldDef]
  Fields 'Complete = WithPos (Map Identifier (WithPos BasicType))

-- | What can appear as the type of a variable.
data BasicType = 
  -- | 'bool' type
  Boolean |
  -- | 'address' type
  Address |
  -- | 'intX' type, where X is converted from bits to bytes for consistency
  -- Note that 'int == int256 == SignedInt 32'
  SignedInt   { bytes :: Natural } |
  -- | 'uintX' type, in bytes
  -- Note that 'uint = uint256 == UnsignedInt 32'
  UnsignedInt { bytes :: Natural } |
  -- | 'bytesX' type.  Natively given in bytes.
  -- Note that 'byte = bytes1 = FixedBytes 1'
  FixedBytes  { bytes :: Natural } |
  -- | 'bytes'.  The length is not known at compile time and the storage
  -- location of the data is not computed here.
  DynamicBytes|
  -- | 'string'.  Same as above.
  String |
  -- | 'T[n]' array type, recording both 'T' and 'n'.  Can be nested; i.e.
  -- 'T[n][m] == FixedArray (FixedArray T n) m'.  Note the order.
  FixedArray  { elemType :: BasicType, fixedLength :: Natural } |
  -- | 'T[]', recording only 'T'.  Can be nested.
  DynamicArray{ elemType :: BasicType } |
  -- | 'mapping(D => C)' type, recording domain 'D' and codomain 'C'.  We
  -- don't enforce restrictions on what these types allow.
  Mapping     { domType  :: BasicType, codType :: BasicType } |
  -- | The name of a new type, whose definition is kept in 'contractTypes'.
  LinkT { linkTo :: LinkID }
  deriving (Eq, Show)

-- | A link is a reference to some non-builtin name in an identifier.  The
-- amount we can deduce about the nature of the referent depends on the
-- stage of parsing.
type TypeLinkStructure = TypeLink 'AfterLayout
type family TypeLink (stage :: Stage) where
  TypeLink 'AfterLayout = DetailedLink 'Complete
  TypeLink 'AfterLinkage = DetailedLink 'Incomplete
  TypeLink s = RoughLink
-- | A rough link does not determine what type is actually referred to.
data RoughLink =
  -- | A plain non-builtin identifier, such as a user-defined type name
  -- (but also possibly a contract)
  UnqualifiedLink Identifier |
  -- | A qualified name A.b, where we don't yet know whether A is
  -- a library, base contract, or qualified imported contract.
  QualifiedLink {
    linkQualifier :: Identifier,
    linkName :: Identifier
    } deriving (Eq, Show, Ord)
-- | The actual things a link can be
data DetailedLink (rstage :: RoughStage) =
  -- | Just a new type name
  PlainLink (DeclLink rstage) |
  -- | A reference to something from a base contract
  InheritedLink (DeclLink rstage) |
  -- | A reference to something from a library contract
  LibraryLink (DeclLink rstage) |
  -- | A reference to an actual contract type
  ContractLink ContractID
-- | When incomplete, we only know the identity of the link; when complete,
-- we also know where it is in storage.  Important for laying out contracts
-- that refer to these links from already-laid out contracts.
type family DeclLink (rstage :: RoughStage) where
  DeclLink 'Incomplete = DeclID
  DeclLink 'Complete = WithPos DeclID
 
-- ID types
-- | Source file names; also source file /paths/.
type FileName = SourceName
-- | Names of contracts.  They have to be the same as identifiers because
-- contracts can also be types.
type ContractName = Identifier
-- | Names of types, variables, functions, etc. in Solidity code.
type Identifier = String
-- | The contract ID needs to record the original file in case of import.
data ContractID = 
  ContractID {
    contractFile :: FileName,
    contractName :: ContractName
    } deriving (Eq, Show, Ord)
-- | The declaration ID needs to record the original contract in case of
-- inheritance or linking.
data DeclID = 
  DeclID {
    declContract :: ContractID,
    declName :: Identifier
    } deriving (Eq, Show, Ord)
-- | The link ID needs to record the original contract and also its name.
data LinkID = 
  LinkID {
    linkContract :: ContractID,
    linkIs :: RoughLink
    } deriving (Eq, Show, Ord)

-- File-level types
-- | A parsed file.  It's important to remember which file contained
-- a contract, because it may be imported multiple times in different ways.
data SolidityFile = 
  SolidityFile {  
    -- | Contracts declared in a file.  The order actually doesn't matter.
    fileContracts :: ContractsByName 'AfterParsing,
    -- | All imports declared in a file.  The order actually doesn't matter.
    fileImports :: [(FileName, ImportAs)]
    }
-- | Mathematically, all of the alternatives below are equivalent to
-- a function 'String -> String'.
data ImportAs =
    -- | Import a name as-is
    Unqualified |
    -- | Import a name in \"object.member\" notation
    StarPrefix ContractName |
    -- | Import a name by an unrelated alias
    Aliases [(ContractName, ContractName)]

-- | We parse directly from the textual source, without pre-lexing.  For
-- the most part, we don't use this type anywhere else.
type SourceCode = String
-- | Alias for a map containing contracts, stored by ID
type ContractsByID (s :: Stage) = Map ContractID (Contract s)
-- | Alias for a map containing contracts, stored by filename then contract
-- name
type ContractsByFile (s :: Stage) = Map FileName (ContractsByName s)
type FileContractsStructure = ContractsByFile 'AfterLayout
-- | Alias for a map containing contracts, stored by name
type ContractsByName (s :: Stage) = Map ContractName (Contract s)
-- | Alias for a map containing files
type SolidityFiles = Map FileName SolidityFile

-- Initial values

-- | To initialize the parser.
emptyContract :: Contract 'AfterParsing
emptyContract = 
  Contract {
    contractTypes = emptyDeclsBy,
    contractStorageVars = [],
    contractBases = [],
    contractLinkage = Map.empty,

    contractRealName = "",
    contractVars = emptyDeclsBy,
    contractFuncs = emptyDeclsBy,
    contractEvents = emptyDeclsBy,
    contractModifiers = emptyDeclsBy,
    contractIsConcrete = True,
    contractIsLibrary = False
    }

-- | For 'emptyContract'
emptyDeclsBy :: DeclarationsBy a
emptyDeclsBy =
  DeclarationsBy {
    byName = Map.empty,
    byID = Map.empty
    }

-- | A convenient accessor
typeSize :: NewType 'AfterLayout -> Natural
typeSize Enum{names} = sizeOf names
typeSize Struct{fields} = sizeOf fields

-- | A convenient predicate
isStorageVar :: VarDef -> Bool
isStorageVar VarDef{varStorage = StorageStorage} = True
isStorageVar _ = False

