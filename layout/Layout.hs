module Layout (
  makeContractsLayout,
  SolidityFileLayout, SolidityContractsLayout,
  SolidityTypesLayout, SolidityVarsLayout,
  SolidityContractLayout(..), SolidityTypeLayout(..),
  SolidityVarLayout(..), LayoutError(..),
  StorageBytes
  ) where

import qualified Data.Map as Map

import Data.Bifunctor
import Data.Maybe

import DefnTypes hiding (IdentT)
import ParserTypes
import LayoutTypes
import Libraries

data LayoutError = 
  LayoutLibraryError LibraryError |
  MissingField Identifier Identifier |
  UnknownType Identifier (Maybe ContractName)

makeContractsLayout :: SolidityContractsDef -> Either LayoutError SolidityContractsLayout
makeContractsLayout contracts = do
  first LayoutLibraryError $ validateLibraries contracts
  let contractsL = sequence $ Map.mapWithKey (\n cD -> (\cL -> makeContractLayout cL n cD) =<< contractsL) contracts
  contractsL

makeContractLayout :: SolidityContractsLayout -> ContractName -> SolidityContractDef
                      -> Either LayoutError SolidityContractLayout
makeContractLayout contractsL name (ContractDef vars types lTypes _ _) = do
  lTypesL <- first LayoutLibraryError $ getLibraryLayouts name contractsL lTypes
  let typesLE = sequence $ Map.mapWithKey (\n t -> (\tL -> makeTypeLayout tL lTypesL n t) =<< typesLE) types
  typesL <- typesLE
  varsL <- makeVarsLayout typesL lTypesL vars
  return $ ContractLayout {
    varsLayout = varsL,
    typesLayout = typesL
    }      

makeTypeLayout :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> Identifier -> SolidityNewType -> Either LayoutError SolidityTypeLayout
makeTypesLayout _ _ _ ContractT = return $ ContractTLayout addressBytes
makeTypesLayout _ _ _ (Enum names)  =
   return $ EnumLayout (ceiling $ logBase (8::Double) $ fromIntegral $ length names)
makeTypeLayout typesL lTypesL tName (Struct fields) = do
  varsLayout <- makeVarsLayout typesL lTypesL fields
  let getObj name = Map.findWithDefault (Left $ MissingField tName name) name
  lastEnd <- varEndBytes <$> (getObj (objName $ last fields) $ Map.map Right varsLayout)
  let usedBytes = nextLayoutStart lastEnd keyBytes        
  return $ StructLayout varsLayout usedBytes

makeVarsLayout :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> [SolidityObjDef] ->
                  Either LayoutError SolidityVarsLayout
makeVarsLayout typesL lTypesL objs = do
  varsLf <- catMaybes <$> mapM (makeVarLayout typesL lTypesL) objs
  let
    varOffEnds = 0:map ((+1) . varEndBytes . snd) varsL
    varsL = zipWith ($) varsLf varOffEnds
  return $ Map.fromList varsL

makeVarLayout :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> SolidityObjDef ->
                 Either LayoutError (Maybe (StorageBytes -> (Identifier, SolidityVarLayout)))
makeVarLayout typesL lTypesL
              ObjDef{objName = name, objValueType = SingleValue t,
                     objArgType = NoValue, objStorage= StorageStorage} = do
  tUsed <- usedBytes typesL lTypesL t
  return $ Just $ \lastOffEnd ->
    let startBytes = nextLayoutStart lastOffEnd tUsed
    in (name,
      VarLayout {
        varStartBytes = startBytes,
        varEndBytes = startBytes + tUsed - 1
        }
      )
makeVarLayout _ _ _ = return Nothing

usedBytes :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> SolidityBasicType ->
             Either LayoutError StorageBytes
usedBytes typesL lTypesL t = case t of
  Boolean -> return 1
  Address -> return addressBytes
  SignedInt b -> return b
  UnsignedInt b -> return b
  FixedBytes b -> return b
  DynamicBytes -> return keyBytes
  String -> return keyBytes
  FixedArray typ l -> do 
    elemSize <- usedBytes typesL lTypesL typ
    let (newEach, numKeys) =
          if elemSize <= 32
          then (32 `quot` elemSize,
                l `quot` newEach + (if l `rem` newEach == 0 then 0 else 1))
          else (1, l * (elemSize `quot` 32)) -- always have rem = 0
    return $ keyBytes * numKeys
  DynamicArray _ -> return keyBytes
  Mapping _ _ -> return $ keyBytes
  Typedef name libM -> typeUsedBytes <$> (getType name $ maybe typesL (lTypesL Map.!) libM)
    where getType name m = Map.findWithDefault (Left $ UnknownType name libM) name $ Map.map Right m

nextLayoutStart :: StorageBytes -> StorageBytes -> StorageBytes
nextLayoutStart 0 _ = 0
nextLayoutStart lastOffEnd thisSize =
  let thisStart0 = lastOffEnd
      lastEnd = lastOffEnd - 1
      thisEnd0 = lastEnd + thisSize
      startKey0 = bytesToKey thisStart0
      endKey0 = bytesToKey thisEnd0
      lastEndKey = bytesToKey lastEnd
      thisStart1 = keyToBytes $ lastEndKey + 1
  in  if (startKey0 == endKey0)
      then thisStart0
      else thisStart1
