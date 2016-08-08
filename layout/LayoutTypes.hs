module LayoutTypes where

import Data.Map (Map)
import Numeric.Natural

import ParserTypes

type SolidityContractsLayout = Map ContractID SolidityContractLayout
type SolidityTypesLayout = Map DeclarationID SolidityTypeLayout
type SolidityVarsLayout = Map DeclarationID SolidityVarLayout

data SolidityContractLayout =
  ContractLayout {
    varsLayout :: SolidityVarsLayout
    typesLayout :: SolidityTypesLayout
    layoutIsLibrary :: Bool
    }

data SolidityVarLayout =
  VarLayout {
    varStartBytes :: StorageBytes,
    varEndBytes :: StorageBytes
    }

data SolidityTypeLayout =
  StructLayout {
    structFieldsLayout :: Map Identifier SolidityVarLayout,
    typeUsedBytes :: StorageBytes
    } |
  EnumLayout {
    typeUsedBytes :: StorageBytes
    } |
  ContractTLayout {
    typeUsedBytes :: StorageBytes
    }

type StorageKey = Natural
type StorageBytes = Natural

addressBytes :: StorageBytes
addressBytes = 20

keyBytes :: StorageBytes
keyBytes = 32

bytesToKey :: StorageBytes -> StorageKey
bytesToKey = (`quot` keyBytes)

keyToBytes :: StorageKey -> StorageBytes
keyToBytes = (* keyBytes)
