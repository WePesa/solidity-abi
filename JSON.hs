{-# LANGUAGE TemplateHaskell #-}

module JSON (makeABISymbols) where

import Data.Aeson.TH
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

import ParserTypes
import SymbolTable

data SymbolTableRowView =
  SymbolTableRowView {
    jsType :: String,
    bytesUsed :: String,
    solidityType :: String,
    atStorageKey :: Maybe String,
    atStorageOffset :: Maybe String,
    arrayDataStart :: Maybe String,
    arrayElement :: Maybe SymbolTableRowView,
    arrayLength :: Maybe String,
    arrayNewKeyEach :: Maybe String,
    enumNames :: Maybe (Map String Integer),
    structFields :: Maybe (Map String SymbolTableRowView),
    mappingValue :: Maybe SymbolTableRowView,
    mappingKey :: Maybe SymbolTableRowView,
    functionDomain :: Maybe [SymbolTableRowView],
    functionArgs :: Maybe [String],
    functionReturns :: Maybe SymbolTableRowView,
    functionHash :: Maybe String
    }

$(deriveToJSON defaultOptions{ omitNothingFields = True } ''SymbolTableRowView)

makeSymTabView :: SymbolTableRow -> SymbolTableRowView
makeSymTabView row@SymbolTableRow{ symbolType = symtype@FunctionType {} } =
  SymbolTableRowView {
    bytesUsed = "0",
    solidityType = functionSignature $ fromJust $ symbolMetadata row,
    jsType = "Function",
    atStorageKey = Nothing,
    atStorageOffset = Nothing,
    arrayDataStart = Nothing,
    arrayElement = Nothing,
    arrayLength = Nothing,
    arrayNewKeyEach = Nothing,
    enumNames = Nothing,
    structFields = Nothing,
    mappingValue = Nothing,
    mappingKey = Nothing,
    functionDomain = Just (map makeSymTabView $ functionArgTypes symtype),
    functionArgs = Just (functionArgNames symtype),
    functionReturns = makeSymTabView <$> (functionRet symtype),
    functionHash = Just (functionSelector $ fromJust $ symbolMetadata row)
    }                     

makeSymTabView row@SymbolTableRow{ symbolType = VariableType {} } =
  let baseView = SymbolTableRowView {
        solidityType = symbolVarType $ symbolType row,
        jsType = genericType $ symbolType row,
        atStorageKey = (toHex . storageKey) <$> storageLocation row,
        atStorageOffset = do
          storageLoc <- storageLocation row
          let sOff = storageValOffset storageLoc
          if sOff /= 0
            then Just $ toHex sOff
            else Nothing,
        bytesUsed = toHex $ storageSize row,
        arrayDataStart = Nothing,
        arrayElement = Nothing,
        arrayLength = Nothing,
        arrayNewKeyEach = Nothing,
        enumNames = Nothing,
        structFields = Nothing,
        mappingValue = Nothing,
        mappingKey = Nothing,
        functionDomain = Nothing,
        functionArgs = Nothing,
        functionReturns = Nothing,
        functionHash = Nothing
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
    _ -> undefined


data ContractSymbolTable = ContractSymbolTable (Map String SymbolTableRowView)
--   ContractABI { contractVariables :: Map String SymbolTableRow,
--                 contractFunctions :: Map String SymbolTableRow }

$(deriveToJSON defaultOptions ''ContractSymbolTable)

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
    (vs, fs) = makeSymbolTable declRows vars
    varRows = Map.fromList (vs ++ fs)
    table =
      let
        isNotContract SymbolTableRow{symbolType = VariableType "contract" _} = False
        isNotContract _ = True
      in (Map.filter isNotContract declRows) `Map.union` varRows
                        
makeABISymbols :: [([SoliditySymbol], SolidityContract)]
                  -> Map String ContractSymbolTable
makeABISymbols declsContracts =
  Map.fromList
  [ makeContractSymbolTable (extDecls, contract) |
    (decls, contract) <- declsContracts,
    let contractToSym Contract{ contractName = name } =
          Variable { varName = name, varType = ContractT }
        extDecls = [ contractToSym c | (_, c) <- declsContracts ] ++ decls ]
