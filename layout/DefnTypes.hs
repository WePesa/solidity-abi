module DefnTypes where

import Data.Map (Map)
import ParserTypes

type IdentT a = Map Identifier a

data SolidityContractDef =
  ContractDef {
    objsDef :: SolidityObjsDef,
    typesDef :: SolidityTypesDef,
    libraryTypes :: [(ContractName, [Identifier])],
    inherits :: [(ContractName, SolidityContractDef)]
    }
type SolidityContractsDef = IdentT SolidityContractDef
type SolidityTypesDef = IdentT SolidityNewType
type SolidityObjsDef = [SolidityObjDef]
