module Layout (
  layout,
  SolidityFileLayout, SolidityContractsLayout,
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
makeContractLayout contracts (ContractDef objs types _) =
  let typesL = Map.map (makeTypeLayout contracts typesL) types
  in ContractLayout {
    objsLayout = makeObjsLayout contracts typesL objs,
    typesLayout = typesL
    }

makeTypeLayout :: SolidityContractsDef -> SolidityTypesLayout -> SolidityNewType
                   -> SolidityTypeLayout
makeTypeLayout contracts typesL t = case t of
  ContractT -> ContractTLayout addressBytes
  Enum names -> EnumLayout (ceiling $ logBase 8 $ fromIntegral $ length names)
  Struct fields ->
    let objsLayout = makeObjsLayout contracts typesL fields
        lastEnd = objEndBytes $ objsLayout Map.! (objName $ last fields)
        usedBytes = nextLayoutStart lastEnd keyBytes        
    in StructLayout objsLayout usedBytes

makeObjsLayout :: SolidityContractsDef -> SolidityTypesLayout -> [SolidityObjDef]
                  -> SolidityObjsLayout
makeObjsLayout contracts typesL objs =
  let objsLf = catMaybes $ map (makeObjLayout contracts typesL) objs
      objEnds = 0:map (objEndBytes . snd) objsL
      objsL = zipWith ($) objsLf objEnds
  in Map.fromList  objsL

makeObjLayout :: SolidityContractsDef -> SolidityTypesLayout -> SolidityObjDef
                 -> Maybe (StorageBytes -> (Identifier, SolidityObjLayout))
makeObjLayout contracts typesL obj = case obj of
  ObjDef{objName = name, objArgType = NoValue, objValueType = SingleValue t} ->
    Just $ \lastEnd ->
    let startBytes = nextLayoutStart lastEnd $ usedBytes t
    in (name,
        ObjLayout {
          objStartBytes = startBytes,
          objEndBytes = startBytes + usedBytes t - 1
          })
  _ -> Nothing
  where
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
      Typedef name -> typeUsedBytes $ Map.findWithDefault err name typesL
        where err = error $
                    "Name " ++ name ++ " is not a user-defined type or contract"


nextLayoutStart :: StorageBytes -> StorageBytes -> StorageBytes
nextLayoutStart 0 _ = 0
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
