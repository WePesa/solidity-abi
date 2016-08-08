module ParserTypes where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bifunctor
import Text.Parsec
import Numeric.Natural

type SolidityParser = Parsec SourceCode SolidityContract

initContract :: ContractID -> SolidityParser ()
initContract cID = putState $ emptyContract cID

addVar :: SolidityVarDef -> SolidityParser ()
addVar v = modifyState $
  \c@Contract{contractOwnDeclarations = cDs@Declarations{declaredVars = vs}} =
    c@Contract{contractOwnDeclarations = cDs{declaredVars = v:vs}}

addFunc :: SolidityFuncDef -> SolidityParser ()
addFunc f = modifyState $
  \c@Contract{contractOwnDeclarations = cDs@Declarations{declaredFuncs = fs}} =
    c@Contract{contractOwnDeclarations = cDs{declaredFuncs = Map.insert (funcID f) f fs}}

addEvent :: SolidityEventDef -> SolidityParser ()
addEvent e = modifyState $ 
  \c@Contract{contractOwnDeclarations = cDs@Declarations{declaredEvents = es}} =
    c@Contract{contractOwnDeclarations = cDs{declaredEvents = Map.insert (eventID e) e es}}

addModifier :: SolidityModifierDef -> SolidityParser ()
addModifier m = modifyState $
  \c@Contract{contractOwnDeclarations = cDs@Declarations{declaredModifiers = ms}} =
    c@Contract{contractOwnDeclarations = cDs{declaredModifiers = Map.insert (modID m) m ms}}

addType :: SolidityTypeDef -> SolidityParser ()
addType t = modifyState $
  \c@Contract{contractOwnDeclarations = cDs@Declarations{declaredTypes = ts}} =
    c@Contract{contractOwnDeclarations = cDs{declaredTypes = Map.insert (typeID t) t ts}}

addLibraryType :: ContractName -> Identifier -> SolidityParser ()
addLibraryType n t = modifyState $
  \c@Contract{contractOwnDeclarations = cDs@Declarations{declaredLibraryTypes = lts}} =
    c{contractOwnDeclarations =
      cDs{declaredLibraryTypes =
        Map.alter (maybe (Just Set.empty) (Just . Set.insert t)) n lts
        }
      }

addBase :: ContractName -> SourceCode -> SolidityParser ()
addBase n x = modifyState $
  \c@Contract{contractInheritancePaths = UnresolvedBases l} =
    c{contractInheritancePaths = UnresolvedBases $ (n,x) : l}

setIsLibrary :: Bool -> SolidityParser ()
setIsLibrary b = modifyState $ \c -> c{contractIsLibrary = b}

emptyContract :: ContractID -> SolidityContract
emptyContract cID = 
  Contract {
    contractID = cID,
    contractOwnDeclarations = emptyDeclarations cID,
    contractAllDeclarations = [],
    contractInheritancePaths = emptyInheritanceMap
    contractIsLibrary = False
    }

emptyInheritanceMap :: InheritanceMap
emptyInheritanceMap = UnresolvedBases []

emptyDeclarations :: ContractID -> SolidityDeclarations
emptyDeclarations cID =
  Declarations {
    declarationsRealContract = cID,
    declaredVars = [],
    declaredFuncs = Map.empty,
    declaredEvents = Map.empty,
    declaredModifiers = Map.empty,
    declaredTypes = Map.empty,
    declaredLibraryTypes = Map.empty
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
    fileContracts :: SolidityContracts,
    fileImports :: [(FileName, ImportAs)]
    } deriving (Eq)

type SolidityFiles = Map FileName SolidityFile
type SolidityContracts = Map ContractName SolidityContract
type SolidityTypes = Map Identifier SolidityTypeDef
type SolidityVars = Map Identifier SolidityVarDef
type SolidityFuncs = Map Identifier SolidityFuncDef
type SolidityEvents = Map Identifier SolidityEventDef
type SolidityModifiers = Map Identifier SolidityModifierDef
type SolidityLibraryTypes = Map ContractName (Set Identifier)

data ContractID =
  ContractID {
    contractRealFile :: FileName,
    contractRealName :: ContractName
    } deriving (Eq, Ord)

data InheritanceMap =
  InheritanceMap {
    -- Ordered with the most derived contracts first
    inheritanceBases :: [(ContractName, InheritanceMap)],
    inheritanceLeaf :: (ContractID, SourceCode)
    } |
  UnresolvedBases [(ContractName, SourceCode)]

data SolidityContract =
  Contract {
    contractID :: ContractID,
    contractOwnDeclarations :: ContractDeclarations,
    -- Ordered with most derived contracts' declarations first
    contractAllDeclarations :: [ContractDeclarations],
    contractInheritancePaths :: InheritanceMap,
    contractIsLibrary :: Bool
    } deriving (Eq)

data ContractDeclarations =
  Declarations {
    declarationsRealContract :: ContractID,
    -- In order of increasing storage location
    declaredVars :: [SolidityVarDef],
    declaredFuncs :: SolidityFuncs,
    declaredEvents :: SolidityEvents,
    declaredModifiers :: SolidityModifiers,
    declaredTypes :: SolidityTypes,
    declaredLibraryTypes :: SolidityLibraryTypes
    } deriving (Eq)

data SolidityVarDef =
  VarDef {
    varID :: Identifier,
    varVisibility :: SolidityVisibility,
    varAssignment :: String,
    varType :: SolidityBasicType,
    varStorage :: SolidityStorage
    } deriving (Eq)

data SolidityFuncDef =
  FuncDef {
    funcID :: Identifier,
    funcVisibility :: SolidityVisibility,
    funcDefn :: String,
    funcValueType :: SolidityTuple,
    funcArgType :: SolidityTuple,
    funcIsConstant :: Bool
    } deriving (Eq)

data SolidityEventDef =
  EventDef {
    eventID :: Identifier,
    eventTopics :: SolidityTuple,
    eventIsAnonymous :: Bool
    } deriving (Eq)

data SolidityModifierDef =
  ModifierDef {
    modID :: Identifier,
    modDefn :: String,
    modArgs :: SolidityTuple
    } deriving (Eq)

data SolidityVisibility = PublicVisible | PrivateVisible | InternalVisible | ExternalVisible deriving (Eq)

data SolidityStorage = StorageStorage | MemoryStorage | IndexedStorage | ValueStorage deriving (Eq)
           
newtype SolidityTuple = TupleValue [SolidityVarDef] deriving (Eq)

data SolidityTypeDef =
  TypeDef {
    typeID :: Identifier,
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
  
