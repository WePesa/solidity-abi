{-# LANGUAGE TemplateHaskell #-}

module JSON where

import Data.Aeson.TH

import SymbolTable

$(deriveToJSON defaultOptions{ omitNothingFields = True } ''SymbolTableRowView)
$(deriveToJSON defaultOptions ''ContractSymbolTable)
