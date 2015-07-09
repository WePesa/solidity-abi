module SymbolTable (
  SymbolTableRowView(..),
  ContractSymbolTable(..),
  makeContractSymbolTable
  ) where

import Blockchain.ExtWord (Word256)
import qualified Crypto.Hash.SHA3 as SHA3
import Data.Bifunctor
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
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
  NoReference | -- Fixed-size types
  UndeterminedReference | -- Mapping types: reference depends on key
  AddressReference String -- Dynamic arrays

data StorageLocation =
  StorageLocation {
    storageKey :: Integer,
    storageValOffset :: Integer, -- Variables are tightly packed
    dataReference :: StorageReference
    }

data SymbolMetadata =
  EnumMetadata { enumNames0 :: Map String Integer } |
  StructMetadata { fieldsTable :: Map String SymbolTableRow } |
  ArrayMetadata { elementStorage :: Maybe SymbolTableRow,
                  arrayLen :: Maybe Integer,
                  newKeyAfterEvery :: Integer } |
  MappingMetadata { valueStorage :: SymbolTableRow,
                    keyStorage :: SymbolTableRow } |
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

initSymbolTableRow :: Map String SymbolTableRow -> SoliditySymbol
                      -> (String, SymbolTableRow)
initSymbolTableRow _ sym@Function{ funcName = name, args = fArgs, returns = ret } =
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
    makeFunctionSelector = concatMap toHex . BS.unpack .
      BS.take 4 . BS.fromStrict . SHA3.hash 256 . BS.toStrict . canonicalSignature 
         
initSymbolTableRow decls sym@Variable{ varName = name, varType = vType } =
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
                    arrayLen = Nothing,
                    newKeyAfterEvery = 32 },
                 defaultDataReference)
      SignedInt b -> (b, Nothing, NoReference)
      UnsignedInt b -> (b, Nothing, NoReference)
      FixedBytes b -> (b, Nothing, NoReference)
      DynamicBytes ->
        let elemRow =
              snd $ head $ makeVariableSymbolTable decls
              [Variable { varName = "", varType = FixedBytes 1 }]
        in (32, Just $ ArrayMetadata { elementStorage = Just elemRow,
                                       arrayLen = Nothing,
                                       newKeyAfterEvery = 32 },
            defaultDataReference)
      SignedReal b _ -> (b, Nothing, NoReference)
      UnsignedReal b _ -> (b, Nothing, NoReference)
      FixedArray t l ->
        let elemRow = snd $ head $ makeVariableSymbolTable decls
                      [Variable { varName = "", varType = t }]
            elemSize = storageSize elemRow
        in (l * elemSize, Just $
                          ArrayMetadata { elementStorage = Just elemRow,
                                          arrayLen = Just l,
                                          newKeyAfterEvery = 32 `quot` elemSize },
            NoReference)
      DynamicArray t ->
        let elemRow = snd $ head $ makeVariableSymbolTable decls
                      [Variable { varName = "", varType = t }]
            elemSize = storageSize elemRow
        in (32, Just $ ArrayMetadata {
               elementStorage = Just elemRow{ storageLocation = Nothing },
               arrayLen = Nothing,
               newKeyAfterEvery = 32 `quot` elemSize },
            defaultDataReference)
      Mapping d t ->
        let valRow = snd $ head $ makeVariableSymbolTable decls
                     [Variable { varName = "", varType = t }]
            keyRow = snd $ head $ makeVariableSymbolTable decls
                     [Variable { varName = "", varType = d }]
        in (32, Just $ MappingMetadata {
               valueStorage = valRow{ storageLocation = Nothing},
               keyStorage = keyRow{ storageLocation = Nothing} }, 
            UndeterminedReference) 
      Enum names ->
        (ceiling $ logBase 8 $ fromIntegral $ length names,
         Just $ EnumMetadata { enumNames0 = Map.fromList $ zip names [0 .. ] },
         NoReference)
      Struct fields ->
        let fieldRows = makeVariableSymbolTable decls fields
        in (sum $ map (storageSize . snd) fieldRows,
            Just $ StructMetadata { fieldsTable = Map.fromList fieldRows },
            NoReference)
      UserDefined name ->
        let Just realTypeRow = Map.lookup name decls
        in (storageSize realTypeRow, Nothing, NoReference)

makeVariableSymbolTable :: Map String SymbolTableRow -> [SoliditySymbol]
                           -> [(String, SymbolTableRow)]
makeVariableSymbolTable decls vars = fst $ makeSymbolTable decls vars

makeSymbolTable :: Map String SymbolTableRow -> [SoliditySymbol]
                   -> ([(String, SymbolTableRow)], [(String, SymbolTableRow)])
makeSymbolTable decls syms = bimap makeStorage (map noStorage) varsFuncs
  where
    varsFuncs = partition isVariable syms
    isVariable (Variable {}) = True
    isVariable _ = False

    noStorage sym = (name, row{ storageLocation = Nothing })
      where (name, row) = initSymbolTableRow decls sym

    makeStorage = scanl1 addStorage . map (initSymbolTableRow decls)
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

getArrayReference :: Integer -> String
getArrayReference key =
  toHex (decode $ BS.fromStrict $ SHA3.hash 256 $ BS.toStrict $
  BS.take 32 $ BS.append (encode key) zeros :: Word256)
  where zeros = BS.pack $ repeat 0

toHex :: (Integral a, Show a) => a -> String
toHex = ("0x" ++) . flip showHex ""

data SymbolTableRowView =
  SymbolTableRowView {
    bytesUsed :: String,
    solidityType :: String,
    atStorageKey :: Maybe String,
    arrayDataStart :: Maybe String,
    arrayElement :: Maybe SymbolTableRowView,
    arrayLength :: Maybe String,
    arrayNewKeyEach :: Maybe String,
    enumNames :: Maybe (Map String Integer),
    structFields :: Maybe (Map String SymbolTableRowView),
    mappingValue :: Maybe SymbolTableRowView,
    mappingKey :: Maybe SymbolTableRowView
    }

makeSymTabView :: SymbolTableRow -> SymbolTableRowView
makeSymTabView row =
  let baseView = SymbolTableRowView {
        solidityType = symbolVarType $ symbolType row,
        atStorageKey = do
          storageLoc <- storageLocation row
          let sKey = storageKey storageLoc
              sOff = storageValOffset storageLoc
          return $ toHex sKey ++
            if sOff /= 0
            then "+" ++ toHex sOff
            else "",
        bytesUsed = toHex $ storageSize row,
        arrayDataStart = Nothing,
        arrayElement = Nothing,
        arrayLength = Nothing,
        arrayNewKeyEach = Nothing,
        enumNames = Nothing,
        structFields = Nothing,
        mappingValue = Nothing,
        mappingKey = Nothing
        }                       
  in case symbolMetadata row of
    Nothing -> baseView
    Just EnumMetadata {enumNames0 = namesMap} ->
      baseView {
        enumNames = Just namesMap,
        atStorageKey = Nothing
        }
    Just StructMetadata {fieldsTable = fieldRowMap} ->
      baseView {
        atStorageKey = Nothing,
        structFields = Just $ Map.map makeSymTabView fieldRowMap
        }
    Just ArrayMetadata { elementStorage = eltRowM, arrayLen = lengthM,
                         newKeyAfterEvery = keyEvery } ->
      baseView {
        arrayElement = makeSymTabView <$> eltRowM,
        arrayLength = toHex <$> lengthM,
        arrayNewKeyEach = Just $ toHex keyEvery,
        arrayDataStart = case dataReference <$> storageLocation row of
          Just (AddressReference s) -> Just s
          _ -> Nothing
        }
    Just MappingMetadata { valueStorage = valRow,
                           keyStorage = keyRow } ->
      baseView {
        mappingValue = Just $ makeSymTabView valRow,
        mappingKey = Just $ makeSymTabView keyRow
        }

data ContractSymbolTable = ContractSymbolTable (Map String SymbolTableRowView)
--   ContractABI { contractVariables :: Map String SymbolTableRow,
--                 contractFunctions :: Map String SymbolTableRow }

makeContractSymbolTable :: ([SoliditySymbol], SolidityContract)
                           -> (String, ContractSymbolTable)
makeContractSymbolTable
  (decls, Contract{contractName = name, contractABI = vars}) =
  (name, ContractSymbolTable $ Map.map makeSymTabView table)
   -- ContractABI {
   --   contractVariables = Map.fromList vars,
   --   contractFunctions = Map.fromList funcs
   --   })
  where
    declRows = Map.fromList $ map (initSymbolTableRow declRows) decls
    varRows = Map.fromList $ makeVariableSymbolTable declRows vars
    table = declRows `Map.union` varRows
                        
