module Layout (
  makeContractsLayout,
  SolidityFileLayout, SolidityContractsLayout,
  SolidityTypesLayout, SolidityVarsLayout,
  SolidityContractLayout(..), SolidityTypeLayout(..),
  SolidityVarLayout(..),
  StorageBytes
  ) where

import qualified Data.Map as Map

import Data.Bifunctor
import Data.Maybe

import DefnTypes
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
  let contractsL = sequence $ Map.mapWithKey (makeContractLayout contractsL) contracts
  contractsL

makeContractLayout :: SolidityContractsLayout -> SolidityContractDef -> ContractName
                      -> Either LayoutError SolidityContractLayout
makeContractLayout contractsL (ContractDef vars types lTypes _) name = do
  lTypesL <- first LayoutLibraryError <$> getLibraryLayouts name contractsL lTypes
  let typesL = sequence $ Map.mapWithKey (makeTypeLayout typesL lTypesL) types
  return $ ContractLayout {
    varsLayout = makeVarsLayout typesL lTypesL vars,
    typesLayout = typesL
    }      

makeTypeLayout :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> SolidityNewType ->
                  Identifier -> Either LayoutError SolidityTypeLayout
makeTypesLayout _ _ ContractT _ = return $ ContractTLayout addressBytes
makeTypesLayout _ _ (Enum names) _ =
   return $ EnumLayout (ceiling $ logBase (8::Double) $ fromIntegral $ length names)
makeTypeLayout typesL lTypesL (Struct fields) tName = do
  varsLayout <- makeVarsLayout typesL lTypesL fields
  let getObj name = Map.findWithDefault (Left $ MissingField tName name) name
  lastEnd <- varEndBytes <$> getObj (varName $ last fields) varsLayout 
  let usedBytes = nextLayoutStart lastEnd keyBytes        
  return $ StructLayout varsLayout' usedBytes

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
              ObjDef{varName = name, varValueType = SingleValue t,
                     varArgType = NoValue, varStorage= StorageStorage} = do
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
  FixedArray typ l -> return $ keyBytes * numKeys
    where
      elemSize = usedBytes typesL lTypesL typ
      (newEach, numKeys) =
        if elemSize <= 32
        then (32 `quot` elemSize,
              l `quot` newEach + (if l `rem` newEach == 0 then 0 else 1))
        else (1, l * (elemSize `quot` 32)) -- always have rem = 0
  DynamicArray _ -> return keyBytes
  Mapping _ _ -> return $ keyBytes
  Typedef name libM -> typeUsedBytes <$> getType name $ maybe typesL (lTypesL Map.!) libM
    where getType name = Map.findWithDefault $ Left $ UnknownType name libM

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
