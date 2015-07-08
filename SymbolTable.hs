module SymbolTable (
  StorageSymbol(..), StorageLocation(..), SymbolMetadata(..), SymbolTableRow(..),
  ContractABI(..), makeContractABI
  ) where

import qualified Crypto.Hash.SHA3 as SHA3
import Data.Bifunctor
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.List
import Data.Maybe
import qualified Data.Map as Map hiding (Map)
import Data.Map (Map)
import Numeric

import Pretty
import ParserTypes

import Debug.Trace

data StorageSymbol =
  VariableSymbol { symbolName :: String, symbolType :: String } |
  FunctionSymbol { symbolName :: String, symbolArgs :: [String], symbolRet :: String }

toStorageSymbol :: SoliditySymbol -> StorageSymbol
toStorageSymbol Variable{ varName = name, varType = vType } =
  VariableSymbol{ symbolName = name, symbolType = show $ pretty vType }
toStorageSymbol Function{ funcName = name, args = aTypes, returns = rType} =
  FunctionSymbol{ symbolName = name, symbolArgs = map (show . pretty) aTypes,
                  symbolRet = maybe "" (show . pretty) rType }

data StorageLocation =
  StorageLocation {
    storageKey :: Integer,
    storageValOffset :: Integer, -- Variables are tightly packed
    {- A dynamic array has Just (Right addr) here, where addr is the location of the array data.
       A mapping has Just (Left ()) here, because its values are not stored at fixed locations.
       All other types have Nothing. -}
    dataReference :: Maybe (Either () Integer)
    }

data SymbolMetadata =
  EnumMetadata { enumNames :: Map String Integer } |
  StructMetadata { fieldsTable :: [SymbolTableRow] } | -- Relative symbol table
  {- In thse two, the {element|value}Storage contains a representative symbol table row
     for an element of the array or mapping.  Its storageLocation is Nothing, since that
     cannot be determined in advance. However, if it is a complex type (say, a struct)
     then its own nested rows *will* have the correct relative storage locations. -}
  ArrayMetadata { elementStorage :: Maybe SymbolTableRow,
                  arrayLength :: Maybe Integer,
                  newKeyAfterEvery :: Integer } |
  MappingMetadata { valueStorage :: SymbolTableRow } |
  FunctionMetadata { functionSelector :: String } -- For message calls

data SymbolTableRow =
  SymbolTableRow {
    storageSymbol :: StorageSymbol,
    storageLocation :: Maybe StorageLocation,
    storageSize :: Integer,
    symbolMetadata :: Maybe SymbolMetadata
    }

initSymbolTableRow :: SoliditySymbol -> SymbolTableRow
initSymbolTableRow sym@Function{} =
  SymbolTableRow {
    storageSymbol = toStorageSymbol sym,
    storageSize = 0,
    symbolMetadata = Just $
      FunctionMetadata {
        functionSelector = makeFunctionSelector sym
        }
    }
  where
    makeFunctionSelector = concatMap (flip showHex "") . BS.unpack .
      BS.take 4 . BS.fromStrict . SHA3.hash 256 . BS.toStrict . canonicalSignature 
         
initSymbolTableRow sym@Variable{varType = vType} =
  SymbolTableRow {
    storageSymbol = toStorageSymbol sym,
    storageLocation =
      Just $ StorageLocation {
        storageKey = 0,
        storageValOffset = 0,
        dataReference = symDefaultDataReference
        },
    storageSize = symSize,
    symbolMetadata = symMetadata
    }
  where
    defaultDataReference = Just $ Right $ getArrayReference 0
    (symSize, symMetadata, symDefaultDataReference) = case vType of
      Boolean -> (1, Nothing, Nothing)
      Address -> (20, Nothing, Nothing)
      String -> (32, Just $ ArrayMetadata {
                    elementStorage = Nothing,
                    arrayLength = Nothing,
                    newKeyAfterEvery = 32 },
                 defaultDataReference)
      SignedInt b -> (b, Nothing, Nothing)
      UnsignedInt b -> (b, Nothing, Nothing)
      FixedBytes b -> (b, Nothing, Nothing)
      DynamicBytes ->
        let elemRow =
              head $ makeVariableSymbolTable
              [Variable { varName = "", varType = FixedBytes 1 }]
        in (32, Just $ ArrayMetadata { elementStorage = Just elemRow,
                                       arrayLength = Nothing,
                                       newKeyAfterEvery = 32 },
            defaultDataReference)
      SignedReal b _ -> (b, Nothing, Nothing)
      UnsignedReal b _ -> (b, Nothing, Nothing)
      FixedArray t l ->
        let elemRow = head $ makeVariableSymbolTable
                      [Variable { varName = "", varType = t }]
            elemSize = storageSize elemRow
        in (l * elemSize, Just $
                          ArrayMetadata { elementStorage = Just elemRow,
                                          arrayLength = Just l,
                                          newKeyAfterEvery = 32 `quot` elemSize },
            Nothing)
      DynamicArray t ->
        let elemRow = head $ makeVariableSymbolTable
                      [Variable { varName = "", varType = t }]
            elemSize = storageSize elemRow
        in (32, Just $ ArrayMetadata {
               elementStorage = Just elemRow{ storageLocation = Nothing },
               arrayLength = Nothing,
               newKeyAfterEvery = 32 `quot` elemSize },
            defaultDataReference)
      Mapping d t ->
        let valRow = head $ makeVariableSymbolTable
                     [Variable { varName = "", varType = t }]
        in (32, Just $ MappingMetadata {
               valueStorage = valRow{ storageLocation = Nothing} }, 
            Just $ Left ()) 
      Enum names ->
        (ceiling $ logBase 8 $ fromIntegral $ length names,
         Just $ EnumMetadata { enumNames = Map.fromList $ zip names [0 .. ] },
         Nothing)
      Struct fields ->
        let fieldRows = makeVariableSymbolTable fields
        in (sum $ map storageSize fieldRows,
            Just $ StructMetadata { fieldsTable = fieldRows },
            Nothing)

makeVariableSymbolTable :: [SoliditySymbol] -> [SymbolTableRow]
makeVariableSymbolTable = fst . makeSymbolTable

makeSymbolTable :: [SoliditySymbol] -> ([SymbolTableRow], [SymbolTableRow])
makeSymbolTable syms = bimap makeStorage (map noStorage) varsFuncs
  where
    varsFuncs = partition isVariable syms
    isVariable (Variable {}) = True
    isVariable _ = False

    noStorage sym = (initSymbolTableRow sym) { storageLocation = Nothing }

    makeStorage = scanl1 addStorage . map initSymbolTableRow
    addStorage row row' =
      let
        Just rowStorage = storageLocation row
        dr0 = dataReference rowStorage
        off0 = storageValOffset rowStorage
        newOff0 = off0 + storageSize row
        nextOff = newOff0 + storageSize row'
        key0 = storageKey rowStorage
        (newKey, newOff) =
          if nextOff > 32 || newOff0 == 32 then (key0 + 1, 0) else (key0, newOff0)
      in row' {
        storageLocation = Just $
           StorageLocation {
             storageKey = newKey,
             storageValOffset = newOff,
             dataReference = case dr0 of
               Just (Right _) -> Just $ Right $ getArrayReference newKey
               _ -> dr0
             }
        }

getArrayReference :: Integer -> Integer
getArrayReference key =
  fromIntegral (decode $ BS.fromStrict $ SHA3.hash 256 $ BS.toStrict $
  BS.take 32 $ BS.append (encode key) zeros :: Word32)
  where zeros = BS.pack $ repeat 0

data ContractABI =
  ContractABI { contractABIName :: String,
                contractVariables :: [SymbolTableRow],
                contractFunctions :: [SymbolTableRow] }

makeContractABI :: SolidityContract -> ContractABI
makeContractABI Contract{contractName = name, contractABI = abi} =
  ContractABI {
    contractABIName = name,
    contractVariables = vars,
    contractFunctions = funcs
    }
  where (vars, funcs) = makeSymbolTable abi
                        
