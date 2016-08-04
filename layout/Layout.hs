{-# LANGUAGE RecursiveDo #-}
module Layout (
  makeFileLayout,
  SolidityFileLayout, SolidityContractsLayout,
  SolidityTypesLayout, SolidityVarsLayout,
  SolidityContractLayout(..), SolidityTypeLayout(..),
  SolidityVarLayout(..), LayoutError(..),
  StorageBytes
  ) where

import qualified Data.Map as Map

import Control.Monad.Fix

import Data.Functor.Identity

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

makeFileLayout :: SolidityContractsDef -> SolidityContractsLayout
makeFileLayout contracts = runIdentity $ mdo
  return $ either (error "Library layout error") id $ first LayoutLibraryError $ validateLibraries contracts
  contractsL <- return $ makeContractsLayout contractsL contracts
  return contractsL

makeContractsLayout :: SolidityContractsLayout -> SolidityContractsDef -> SolidityContractsLayout
makeContractsLayout contractsL contracts = Map.mapWithKey (makeContractLayout contractsL) contracts

makeContractLayout :: SolidityContractsLayout -> ContractName -> SolidityContractDef
                      -> SolidityContractLayout
makeContractLayout contractsL name (ContractDef vars types lTypes _ _) = runIdentity $ mdo
  lTypesL <- return $ error ("Library layout error") id $ first LayoutLibraryError $ getLibraryLayouts name contractsL lTypes
  typesL <- return $ makeTypesLayout typesL lTypesL types
  varsL <- return $ makeVarsLayout typesL lTypesL vars
  return $ ContractLayout {
    varsLayout = varsL,
    typesLayout = typesL
    }      

makeTypesLayout :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> SolidityTypesDef -> SolidityTypesLayout
makeTypesLayout typesL lTypesL types = Map.mapWithKey (makeTypeLayout typesL lTypesL) types

makeTypeLayout :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> Identifier -> SolidityNewType -> SolidityTypeLayout
makeTypeLayout _ _ _ ContractT = ContractTLayout addressBytes
makeTypeLayout _ _ _ (Enum names)  =
   EnumLayout (ceiling $ logBase (256::Double) $ fromIntegral $ length names)
makeTypeLayout typesL lTypesL tName (Struct fields) = runIdentity $ do
  varsLayout <- return $ makeVarsLayout typesL lTypesL fields
  let getObj name = Map.findWithDefault (Left $ MissingField tName name) name
  lastEnd <- return $ either (error "Missing field error") id $ varEndBytes <$> (getObj (objName $ last fields) $ Map.map Right varsLayout)
  let usedBytes = nextLayoutStart lastEnd keyBytes        
  return $ StructLayout varsLayout usedBytes

makeVarsLayout :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> [SolidityObjDef] ->
                  SolidityVarsLayout
makeVarsLayout typesL lTypesL objs = runIdentity $ do
  varsLf <- return $ catMaybes $ map (makeVarLayout typesL lTypesL) objs
  let
    varOffEnds = 0:map ((+1) . varEndBytes . snd) varsL
    varsL = zipWith ($) varsLf varOffEnds
  return $ Map.fromList varsL

makeVarLayout :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> SolidityObjDef ->
                 (Maybe (StorageBytes -> (Identifier, SolidityVarLayout)))
makeVarLayout typesL lTypesL
              ObjDef{objName = name, objValueType = SingleValue t,
                     objArgType = NoValue, objStorage= StorageStorage} = runIdentity $ do
  tUsed <- return $ usedBytes typesL lTypesL t
  return $ Just $ \lastOffEnd ->
    let startBytes = nextLayoutStart lastOffEnd tUsed
    in (name,
      VarLayout {
        varStartBytes = startBytes,
        varEndBytes = startBytes + tUsed - 1
        }
      )
makeVarLayout _ _ _ = Nothing

usedBytes :: SolidityTypesLayout -> IdentT SolidityTypesLayout -> SolidityBasicType ->
             StorageBytes
usedBytes typesL lTypesL t = runIdentity $ case t of
  Boolean -> return 1
  Address -> return addressBytes
  SignedInt b -> return b
  UnsignedInt b -> return b
  FixedBytes b -> return b
  DynamicBytes -> return keyBytes
  String -> return keyBytes
  FixedArray typ l -> do 
    elemSize <- return $ usedBytes typesL lTypesL typ
    let (newEach, numKeys) =
          if elemSize <= 32
          then (32 `quot` elemSize,
                l `quot` newEach + (if l `rem` newEach == 0 then 0 else 1))
          else (1, l * (elemSize `quot` 32)) -- always have rem = 0
    return $ keyBytes * numKeys
  DynamicArray _ -> return keyBytes
  Mapping _ _ -> return $ keyBytes
  Typedef name libM -> return $ either (error "Unknown type error") id $ typeUsedBytes <$> (getType name $ maybe typesL (lTypesL Map.!) libM)
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
