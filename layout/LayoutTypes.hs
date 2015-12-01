module LayoutTypes where

import qualified Data.Map as Map
import Data.Map (Map)

import Numeric.Natural

import ParserTypes

type IdentT a = Map Identifier a

data SolidityContractDef =
  ContractDef {
    objsDef :: SolidityObjsDef,
    typesDef :: SolidityTypesDef,
    baseNames :: [ContractName]
    }
type SolidityContractsDef = IdentT SolidityContractDef
type SolidityTypesDef = IdentT SolidityNewType
type SolidityObjsDef = [SolidityObjDef]

makeContractsDef :: [SolidityContract] -> SolidityContractsDef
makeContractsDef contracts = Map.fromList $ map contractToDef contracts
  where contractToDef (Contract name objs types bases) =
          (name, ContractDef {
              objsDef = objs,
              typesDef = makeTypesDef $ types ++ contractTypes,
              baseNames = map fst bases
              })
        contractTypes = do
          Contract{contractName = name} <- contracts
          return $ TypeDef name ContractT

makeTypesDef :: [SolidityTypeDef] -> SolidityTypesDef
makeTypesDef types = Map.fromList $ map typeToTuple types
  where typeToTuple (TypeDef name decl) = (name, decl)

type SolidityFileLayout = SolidityContractsLayout
type SolidityContractsLayout = IdentT SolidityContractLayout
type SolidityObjsLayout = IdentT SolidityObjLayout
type SolidityTypesLayout = IdentT SolidityTypeLayout

data SolidityContractLayout =
  ContractLayout {
    objsLayout :: SolidityObjsLayout,
    typesLayout :: SolidityTypesLayout
    }

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
