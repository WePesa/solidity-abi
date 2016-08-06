module ParserTypes where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bifunctor
import Text.Parsec
import Numeric.Natural

type SolidityParser = Parsec SourceCode (ContractName, SolidityContract)

setContractName :: ContractName -> SolidityParser ()
setContractName n = modifyState $ \(_, c) -> (n, c{contractName = n})

addVar :: SolidityVarDef -> SolidityParser ()
addVar o = modifyState $ second $ \c@Contract{contractVars = os} -> c{contractVars = o:os}

addFunc :: SolidityFuncDef -> SolidityParser ()
addFunc o = modifyState $ second $ \c@Contract{contractFuncs = os} -> c{contractFuncs = Map.insert (funcName o) o os}

addEvent :: SolidityEventDef -> SolidityParser ()
addEvent o = modifyState $ second $ \c@Contract{contractEvents = os} -> c{contractEvents = Map.insert (eventName o) o os}

addModifier :: SolidityModifierDef -> SolidityParser ()
addModifier o = modifyState $ second $ \c@Contract{contractModifiers = os} -> c{contractModifiers = Map.insert (modifierName o) o os}

addType :: SolidityTypeDef -> SolidityParser ()
addType t = modifyState $ second $ \c@Contract{contractTypes = ts} -> c{contractTypes = Map.insert (typeName t) t ts}

addLibraryType :: ContractName -> Identifier -> SolidityParser ()
addLibraryType n t = modifyState $
  \c@Contract{contractLibraryTypes = lts} ->
    c{contractLibraryTypes = Map.alter (maybe (Just Set.empty) (Just . Set.insert t)) n lts}

addBase :: ContractName -> SourceCode -> SolidityParser ()
addBase n x = modifyState $ second $ \c@Contract{contractInherits = bs} -> c{contractInherits = (n, x) : bs}

setIsLibrary :: SolidityParser ()
setIsLibrary = modifyState $ second $ \c -> c{contractIsLibrary = True}

emptyContract :: SolidityContract
emptyContract = Contract {
  contractName = "",
  contractVars = [],
  contractFuncs = [],
  contractEvents = [],
  contractModifiers = [],
  contractTypes = [],
  contractLibraryTypes = Map.empty,
  contractInherits = [],
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
    fileContracts :: SolidityContracts
    fileImports :: [(FileName, ImportAs)]
    } deriving (Eq)

type SolidityContracts = Map ContractName SolidityContract
type SolidityTypes = Map Identifier SolidityTypeDef
type SolidityVars = Map Identifier SolidiyVarDef
type SolidityFuncs = Map Identifier SolidityFuncDef
type SolidityEvents = Map Identifier SolidityEventDef
type SolidityModifiers = Map Identifier SolidityModifierDef
type SolidityLibraryTypes = Map ContractName (Set Identifier)

data SolidityContract =
  Contract {
    contractName :: ContractName,
    contractVars :: [SolidityVarDef], -- Must be ordered
    contractFuncs :: SolidityFuncs,
    contractEvents :: SolidityEvents,
    contractModifiers :: SolidityModifiers,
    contractTypes :: SolidityTypes,
    contractLibraryTypes :: SolidityLibraryTypes,
    contractInherits :: [(ContractName, SourceCode)], -- Must be ordered
    contractIsLibrary :: Bool
    } deriving (Eq)

data SolidityVarDef =
  VarDef {
    varName :: Identifier,
    varVisibility :: SolidityVisibility,
    varAssignment :: String,
    varType :: SolidityBasicType,
    varStorage :: SolidityStorage
    } deriving (Eq)

data SolidityFuncDef =
  FuncDef {
    funcName :: Identifier,
    funcVisibility :: SolidityVisibility,
    funcDefn :: String,
    funcValueType :: SolidityTuple,
    funcArgType :: SolidityTuple,
    funcIsConstant :: Bool
    } deriving (Eq)

data SolidityEventDef =
  EventDef {
    eventName :: Identifier,
    eventTopics :: SolidityTuple,
    eventIsAnonymous :: Bool
    } deriving (Eq)

data SolidityModifierDef =
  ModifierDef {
    modName :: Identifier,
    modDefn :: String,
    modArgs :: SolidityTuple
    } deriving (Eq)

data SolidityVisibility = PublicVisible | PrivateVisible | InternalVisible | ExternalVisible deriving (Eq)

data SolidityStorage = StorageStorage | MemoryStorage | IndexedStorage | ValueStorage deriving (Eq)
           
data SolidityTuple = TupleValue [SolidityVarDef] deriving (Eq)

data SolidityTypeDef =
  TypeDef {
    typeName :: Identifier,
    typeDecl :: SolidityNewType
    }
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
  Struct      { fields :: [SolidityVarDef] }
  deriving (Eq)
  
