module SolidityTypes where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bifunctor
import Text.Parsec
import Numeric.Natural

type FileName = SourceName
type SourceCode = String
type Identifier = String
type ContractName = Identifier

data ContractID =
  ContractID {
    contractRealFile :: FileName,
    contractRealName :: ContractName
    } deriving (Eq, Ord)

data DeclarationID = 
  DeclarationID {
    declarationContract :: ContractID,
    declarationRealName :: Identifier,
    } deriving (Eq, Ord)

data ImportAs =
    Unqualified |
    StarPrefix ContractName |
    Aliases [(ContractName, ContractName)]
   
type SolidityFiles = Map FileName SolidityFile
type SolidityContracts = Map ContractName SolidityContract
type ContractsByID = Map ContractID SolidityContract

data SolidityFile = 
  SolidityFile {  
    fileContracts :: SolidityContracts,
    fileImports :: [(FileName, ImportAs)]
    }

data SolidityContract =
  Contract {
    contractID :: ContractID,
    -- In order of decreasing storage location
    contractStorageVars :: [DeclarationID],
    contractDeclarationsByID :: ContractDeclarations DeclarationID,
    contractDeclarationsByName :: ContractDeclarations Identifier,
    contractInherits :: [ContractName],
    contractLibraryTypes :: Map ContractName (Set Identifier),
    contractIsConcrete :: Bool,
    contractIsLibrary :: Bool
    }

data ContractDeclarations a =
  Declarations {
    declaredVars :: Map a SolidityVarDef,
    declaredFuncs :: Map a SolidityFuncDef
    declaredEvents :: Map a SolidityEventDef,
    declaredModifiers :: Map a SolidityModifierDef,
    declaredTypes :: Map a SolidityTypeDef,
    }

data SolidityVarDef =
  VarDef {
    varID :: DeclarationID,
    varVisibility :: SolidityVisibility,
    varAssignment :: String,
    varType :: SolidityBasicType,
    varStorage :: SolidityStorage
    }

data SolidityFuncDef =
  FuncDef {
    funcID :: DeclarationID,
    funcVisibility :: SolidityVisibility,
    funcValueType :: SolidityTuple,
    funcArgType :: SolidityTuple,
    funcHasCode :: Bool,
    funcIsConstant :: Bool
    }

data SolidityEventDef =
  EventDef {
    eventID :: DeclarationID,
    eventTopics :: SolidityTuple,
    eventIsAnonymous :: Bool
    }

data SolidityModifierDef =
  ModifierDef {
    modID :: DeclarationID,
    modDefn :: String,
    modArgs :: SolidityTuple
    }

data SolidityVisibility = PublicVisible | PrivateVisible | InternalVisible | ExternalVisible

data SolidityStorage = StorageStorage | MemoryStorage | IndexedStorage | ValueStorage
           
newtype SolidityTuple = TupleValue [SolidityVarDef]

data SolidityTypeDef =
  TypeDef {
    typeID :: Identifier,
    typeDecl :: SolidityNewType
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
  Typedef     { typedefName :: Identifier, typedefLibrary :: Maybe ContractName }
  
data SolidityNewType =
  Enum        { names  :: [Identifier] } |
  Struct      { fields :: [SolidityVarDef] }
  
