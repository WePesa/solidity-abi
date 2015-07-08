module SymbolTable (
  StorageReference (..), StorageLocation(..),
  SymbolMetadata(..), SymbolType(..), SymbolTableRow(..),
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

data StorageReference =
  NoReference | UndeterminedReference | AddressReference Integer

data StorageLocation =
  StorageLocation {
    storageKey :: Integer,
    storageValOffset :: Integer, -- Variables are tightly packed
    {- A dynamic array has Just (Right addr) here, where addr is the location of the array data.
       A mapping has Just (Left ()) here, because its values are not stored at fixed locations.
       All other types have Nothing. -}
    dataReference :: StorageReference
    }

data SymbolMetadata =
  EnumMetadata { enumNames :: Map String Integer } |
  StructMetadata { fieldsTable :: Map String SymbolTableRow } | -- Relative symbol table
  {- In thse two, the {element|value}Storage contains a representative symbol table row
     for an element of the array or mapping.  Its storageLocation is Nothing, since that
     cannot be determined in advance. However, if it is a complex type (say, a struct)
     then its own nested rows *will* have the correct relative storage locations. -}
  ArrayMetadata { elementStorage :: Maybe SymbolTableRow,
                  arrayLength :: Maybe Integer,
                  newKeyAfterEvery :: Integer } |
  MappingMetadata { valueStorage :: SymbolTableRow } |
  FunctionMetadata { functionSelector :: String } -- For message calls

data SymbolType =
  VariableType { symbolVarType :: String } |
  FunctionType { symbolArgTypes :: [String], symbolReturnType :: String }

data SymbolTableRow =
  SymbolTableRow {
    symbolType :: SymbolType,
    storageLocation :: Maybe StorageLocation,
    storageSize :: Integer,
    symbolMetadata :: Maybe SymbolMetadata
    }

initSymbolTableRow :: SoliditySymbol -> (String, SymbolTableRow)
initSymbolTableRow sym@Function{ funcName = name, args = fArgs, returns = ret } =
  ( name,
    SymbolTableRow {
       symbolType = FunctionType prettyArgs prettyRet,
       storageSize = 0,
       symbolMetadata = Just $
                        FunctionMetadata {
                          functionSelector = makeFunctionSelector sym
                          }
       }
  )
  where
    prettyArgs = map (show . pretty) fArgs
    prettyRet = maybe "" (show . pretty) ret
    makeFunctionSelector = concatMap (flip showHex "") . BS.unpack .
      BS.take 4 . BS.fromStrict . SHA3.hash 256 . BS.toStrict . canonicalSignature 
         
initSymbolTableRow sym@Variable{ varName = name, varType = vType } =
  ( name,
    SymbolTableRow {
       symbolType = VariableType $ show $ pretty vType,
       storageLocation =
         Just $ StorageLocation {
           storageKey = 0,
           storageValOffset = 0,
           dataReference = symDefaultDataReference
           },
       storageSize = symSize,
       symbolMetadata = symMetadata
       }
  )
  where
    defaultDataReference = AddressReference $ getArrayReference 0
    (symSize, symMetadata, symDefaultDataReference) = case vType of
      Boolean -> (1, Nothing, NoReference)
      Address -> (20, Nothing, NoReference)
      String -> (32, Just $ ArrayMetadata {
                    elementStorage = Nothing,
                    arrayLength = Nothing,
                    newKeyAfterEvery = 32 },
                 defaultDataReference)
      SignedInt b -> (b, Nothing, NoReference)
      UnsignedInt b -> (b, Nothing, NoReference)
      FixedBytes b -> (b, Nothing, NoReference)
      DynamicBytes ->
        let elemRow =
              snd $ head $ makeVariableSymbolTable
              [Variable { varName = "", varType = FixedBytes 1 }]
        in (32, Just $ ArrayMetadata { elementStorage = Just elemRow,
                                       arrayLength = Nothing,
                                       newKeyAfterEvery = 32 },
            defaultDataReference)
      SignedReal b _ -> (b, Nothing, NoReference)
      UnsignedReal b _ -> (b, Nothing, NoReference)
      FixedArray t l ->
        let elemRow = snd $ head $ makeVariableSymbolTable
                      [Variable { varName = "", varType = t }]
            elemSize = storageSize elemRow
        in (l * elemSize, Just $
                          ArrayMetadata { elementStorage = Just elemRow,
                                          arrayLength = Just l,
                                          newKeyAfterEvery = 32 `quot` elemSize },
            NoReference)
      DynamicArray t ->
        let elemRow = snd $ head $ makeVariableSymbolTable
                      [Variable { varName = "", varType = t }]
            elemSize = storageSize elemRow
        in (32, Just $ ArrayMetadata {
               elementStorage = Just elemRow{ storageLocation = Nothing },
               arrayLength = Nothing,
               newKeyAfterEvery = 32 `quot` elemSize },
            defaultDataReference)
      Mapping d t ->
        let valRow = snd $ head $ makeVariableSymbolTable
                     [Variable { varName = "", varType = t }]
        in (32, Just $ MappingMetadata {
               valueStorage = valRow{ storageLocation = Nothing} }, 
            UndeterminedReference) 
      Enum names ->
        (ceiling $ logBase 8 $ fromIntegral $ length names,
         Just $ EnumMetadata { enumNames = Map.fromList $ zip names [0 .. ] },
         NoReference)
      Struct fields ->
        let fieldRows = makeVariableSymbolTable fields
        in (sum $ map (storageSize . snd) fieldRows,
            Just $ StructMetadata { fieldsTable = Map.fromList fieldRows },
            NoReference)

makeVariableSymbolTable :: [SoliditySymbol] -> [(String, SymbolTableRow)]
makeVariableSymbolTable = fst . makeSymbolTable

makeSymbolTable :: [SoliditySymbol] -> ([(String, SymbolTableRow)], [(String, SymbolTableRow)])
makeSymbolTable syms = bimap makeStorage (map noStorage) varsFuncs
  where
    varsFuncs = partition isVariable syms
    isVariable (Variable {}) = True
    isVariable _ = False

    noStorage sym = (name, row{ storageLocation = Nothing })
      where (name, row) = initSymbolTableRow sym

    makeStorage = scanl1 addStorage . map initSymbolTableRow
    addStorage (_,row) (name, row') =
      let
        Just rowStorage = storageLocation row
        dr0 = dataReference rowStorage
        off0 = storageValOffset rowStorage
        newOff0 = off0 + storageSize row
        nextOff = newOff0 + storageSize row'
        key0 = storageKey rowStorage
        (newKey, newOff) =
          if nextOff > 32 || newOff0 == 32 then (key0 + 1, 0) else (key0, newOff0)
      in (name, row' {
        storageLocation = Just $
           StorageLocation {
             storageKey = newKey,
             storageValOffset = newOff,
             dataReference = case dr0 of
               AddressReference _ -> AddressReference $ getArrayReference newKey
               _ -> dr0
             }
        })

getArrayReference :: Integer -> Integer
getArrayReference key =
  fromIntegral (decode $ BS.fromStrict $ SHA3.hash 256 $ BS.toStrict $
  BS.take 32 $ BS.append (encode key) zeros :: Word32)
  where zeros = BS.pack $ repeat 0

data ContractABI =
  ContractABI { contractABIName :: String,
                contractVariables :: Map String SymbolTableRow,
                contractFunctions :: Map String SymbolTableRow }

makeContractABI :: SolidityContract -> ContractABI
makeContractABI Contract{contractName = name, contractABI = abi} =
  ContractABI {
    contractABIName = name,
    contractVariables = Map.fromList vars,
    contractFunctions = Map.fromList funcs
    }
  where (vars, funcs) = makeSymbolTable abi
                        
