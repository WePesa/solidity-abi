module Layout (
  layout,
  SolidityFileLayout, SoliditycontractsLayout,
  SolidityTypesLayout, SolidityObjsLayout,
  SolidityContractLayout(..), SolidityTypeLayout(..),
  SolidityObjLayout(..),
  StorageBytes
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import ParserTypes
import LayoutTypes

layout :: SolidityFile -> SolidityFileLayout
layout = makeContractsLayout . makeContractsDef

makeContractsLayout :: SolidityContractsDef -> SolidityContractsLayout
makeContractsLayout contracts = Map.map (makeContractLayout contracts) contracts

makeContractLayout :: SolidityContractsDef -> SolidityContractDef
                      -> SolidityContractLayout
makeContractLayout contracts (ContractDef objs types baseNames) =
  let typesL = Map.map (makeTypeLayout contracts typesL) types
  in ContractLayout {
    objsLayout = makeObjsLayout contracts typesL objs,
    typesLayout = typesL
    }

makeTypeLayout :: SolidityContractsDef -> SolidityTypesLayout -> SolidityTypeDef
                   -> SolidityTypeLayout
makeTypeLayout contracts typesL (TypeDef _ t) = case t of
  ContractT -> ContractTLayout addressBytes
  Enum names -> EnumLayout (ceiling $ logBase 8 $ fromIntegral $ length names)
  Struct _ fields ->
    let objsLayout = makeObjsLayout contracts typesL fields
        lastEnd = objEndBytes $ last objsLayout
        usedBytes = nextLayoutStart lastEnd keyBytes        
    in StructLayout objsLayout usedBytes

makeObjsLayout :: SolidityContractsDef -> SolidityTypesLayout -> [SolidityObjDef]
                  -> SolidityObjsLayout
makeObjsLayout contracts typesL objs =
  let names = map objName objs
      objsL = catMaybes $ zipWith (makeObjLayout contracts typesL) objEnds objs
      objEnds = 0:map objEndBytes objsL
  in Map.fromList $ zip names objsL

makeObjLayout :: SolidityContractsDef -> SolidityTypesLayout -> StorageBytes
                 -> SolidityObjDef -> Maybe SolidityObjLayout
makeObjLayout contracts typesL lastEnd obj = case obj of
  ObjDef{objArgType = NoValue, objValueType = SingleValue t} ->
    Just ObjLayout {
      objStartBytes = startBytes,
      objEndBytes = startBytes + usedBytes t
      }
    where
      startBytes = nextLayoutStart lastEnd usedBytes
      usedBytes ty = case ty of
        Boolean -> 1
        Address -> addressBytes
        SignedInt b -> b
        UnsignedInt b -> b
        FixedBytes b -> b
        DynamicBytes -> keyBytes
        String -> keyBytes
        FixedArray typ l -> keyBytes * numKeys
          where
            elemSize = usedBytes typ
            (newEach, numKeys) =
              if elemSize <= 32
              then (32 `quot` elemSize,
                    l `quot` newEach + (if l `rem` newEach == 0 then 0 else 1))
              else (1, l * (elemSize `quot` 32)) -- always have rem = 0
        DynamicArray _ -> keyBytes
        Mapping _ _ -> keyBytes
        TypeDef name -> typeUsedBytes $ findWithDefault err name typesL
          where err = error $
                      "Name " ++ name ++ " is not a user-defined type or contract"
  _ -> Nothing

nextLayoutStart :: StorageBytes -> StorageBytes -> StorageBytes
nextLayoutStart lastEnd thisSize =
  let thisStart0 = lastEnd + 1
      thisEnd0 = lastEnd + thisSize
      startKey0 = bytesToKey thisStart0
      endKey0 = bytesToKey thisEnd0
      lastEndKey = bytesToKey lastEnd
      thisStart1 = keyToBytes $ lastEndKey + 1
  in  if (startKey0 == endKey0)
      then thisStart0
      else thisStart1

