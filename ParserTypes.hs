module ParserTypes where

import Control.Arrow
import Data.ByteString (ByteString)
import Data.Functor
import qualified Data.Map.Strict as Map hiding (Map)
import Data.Map.Strict (Map)
import Text.Parsec
import Text.PrettyPrint
import Data.List
import Data.Word
  
data ContractDefinitions =
  ContractDefinitions {
    currentContract :: String,
    contractTypes :: Map String SolidityType,
    contractConsts :: Map String SolidityConstant
    }

emptyDefinitions :: ContractDefinitions
emptyDefinitions = ContractDefinitions "" Map.empty Map.empty

clearState :: SolidityParser ()
clearState = putState emptyDefinitions -- will change when we do contract types

type SolidityParser = Parsec String ContractDefinitions

setContractName :: String -> SolidityParser ()
setContractName name = do
  state <- getState
  putState $ state{currentContract = name}

getContractName :: SolidityParser String
getContractName = do
  state <- getState
  return $ currentContract state

addToTypeDefs :: String -> SolidityType -> SolidityParser ()
addToTypeDefs s t = do
  state <- getState
  let types' = Map.insert s t $ contractTypes state
  putState $ state{contractTypes = types'}

getFromTypeDefs :: String -> SolidityParser (Maybe SolidityType)
getFromTypeDefs s = do
  state <- getState
  return $ Map.lookup s (contractTypes state)

addToConsts :: String -> SolidityConstant -> SolidityParser ()
addToConsts s c = do
  state <- getState
  let consts' = Map.insert s c $ contractConsts state
  putState $ state{contractConsts = consts'}

getFromConsts :: String -> SolidityParser (Maybe SolidityConstant)
getFromConsts s = do
  state <- getState
  return $ Map.lookup s (contractConsts state)

data SolidityContract =
  Contract { contractName :: String, contractABI :: [SoliditySymbol] }
  deriving (Show)

data SoliditySymbol =
  Variable { varName :: String, varType :: SolidityType } |
  Function { funcName :: String,
             args :: [SoliditySymbol], returns :: Maybe SolidityType }
  deriving (Show)

data SolidityType =
  Boolean |
  Address |
  SignedInt   { bytes :: Integer } |
  UnsignedInt { bytes :: Integer } |
  FixedBytes  { bytes :: Integer } |
  DynamicBytes|
  String |
  -- SignedReal  { bytes :: Integer, precision :: Integer } |
  -- UnsignedReal{ bytes :: Integer, precision :: Integer } |  
  FixedArray  { elemType :: SolidityType, fixedLength :: Integer } |
  DynamicArray{ elemType :: SolidityType } |
  Mapping     { domType  :: SolidityType, codType :: SolidityType } |
  Enum        { names  :: [String] } |
  Struct      { fields :: [SoliditySymbol] } |
  ContractT |
  UserDefined { typeName :: String }
  deriving (Show)

data SolidityConstant =
  BooleanAs Bool |
  AddressAs Integer |
  StringAs String |
  SignedIntAs Integer |
  UnsignedIntAs Integer |
  FixedBytesAs ByteString |
  DynamicBytesAs ByteString |
  SignedRealAs (Integer, Word) |
  UnsignedRealAs (Integer, Word) |
  EnumAs (Map String Integer)
