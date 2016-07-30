module LayoutTypes where

import Data.Map (Map)
import Numeric.Natural

import ParserTypes

type IdentT a = Map Identifier a

type SolidityFileLayout = SolidityContractsLayout
type SolidityContractsLayout = IdentT SolidityContractLayout
type SolidityObjsLayout = IdentT SolidityObjLayout
type SolidityTypesLayout = IdentT SolidityTypeLayout

data SolidityContractLayout =
  ContractLayout {
    objsLayout :: SolidityObjsLayout,
    typesLayout :: SolidityTypesLayout
    }
  deriving (Show)

data SolidityObjLayout =
  ObjLayout {
    objStartBytes :: StorageBytes,
    objEndBytes :: StorageBytes
    }
  deriving (Show)

data SolidityTypeLayout =
  StructLayout {
    structFieldsLayout :: SolidityObjsLayout,
    typeUsedBytes :: StorageBytes
    } |
  EnumLayout {
    typeUsedBytes :: StorageBytes
    } |
  UsingLayout {
    typeUsedBytes :: StorageBytes
    } |
  ContractTLayout {
    typeUsedBytes :: StorageBytes
    }
  deriving (Show)

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
