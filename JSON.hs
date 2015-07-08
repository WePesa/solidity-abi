{-# LANGUAGE TemplateHaskell #-}

module JSON where

import Data.Aeson.TH

import ParserTypes
import SymbolTable

$(deriveToJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''SolidityType)
$(deriveToJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''SoliditySymbol)
$(deriveToJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''SolidityContract)
$(deriveToJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''StorageSymbol)
$(deriveToJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''StorageLocation)
$(deriveToJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''SymbolMetadata)
$(deriveToJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''SymbolTableRow)
$(deriveToJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''ContractABI)
