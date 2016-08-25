module Layout (doContractLayouts) where

import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.Reader

import SolidityTypes
import LayoutTypes
import DAG

type LayoutReader = Reader SolidityContractsLayout

doContractLayouts :: ContractsByName -> SolidityContractsLayout
doContractLayouts contracts = either (error "Library layout error") id $ do
  validateLibraries contracts
  let result = Map.map (\c -> runReader (doContractLayout c) result) contracts
  return result

validateLibraries :: ContractsByName -> Either (DAGError ContractName) ()
validateLibraries contracts = 
  checkDAG $ Map.map (Map.keys . contractLibraryTypes) contracts

doContractLayout :: SolidityContract -> LayoutReader SolidityContractLayout
doContractLayout contract = do
  varsL <- doVarsLayout (byID $ contractVars contract) (contractStorageVars contract)
  typesL <- doTypesLayout $ contractTypes contract
  return $ ContractLayout {
    varsLayout = varsL,
    typesLayout = typesL,
    layoutIsLibrary = contractIsLibrary contract
    }

doTypesLayout :: Map DeclID SolidityTypeDef -> LayoutReader SolidityTypesLayout
doTypesLayout types = sequence $ Map.map (doTypeLayout . byID) types

doTypeLayout :: SolidityNewType -> LayoutReader SolidityTypeLayout
doTypeLayout (Enum names)  =
  return $ EnumLayout {
    typeUsedBytes = ceiling $ logBase (256::Double) $ fromIntegral $ length names
    }
doTypeLayout (Struct fields) = do
  fieldsLayout <- doVarTypesLayout fields
  let lastEnd = varEndBytes $ fieldsLayout Map.! varName (last fields)
  return $ StructLayout {
    structFieldsLayout = fieldsLayout,
    typeUsedBytes = nextLayoutStart lastEnd keyBytes
    }

doVarsLayout :: Map DeclID SolidityVarDef -> [DeclID] -> LayoutReader SolidityVarsLayout
doVarsLayout varDefs varIDs = doVarTypesLayout $ map (varType . (varDefs Map.!)) varIDs

doVarTypesLayout :: [SolidityBasicType] -> LayoutReader SolidityVarsLayout
doVarTypesLayout varTs = do
  -- vars are listed from high storage to low; we work from the right
  varsL <- foldr (\v vLs -> makeNextVarL v vLs) (return []) varTs 
  return $ makeVarsMap varsL
  where
    makeVarsMap vLs = Map.fromList $ zipWith (\v vL -> (varName v, vL)) vars vLs
    makeNextVarL v vLsR = do
      vLs <- vLsR
      case vLs of
        [] -> sequence [doVarLayout 0 v]
        (vL : rest) -> do
          vL' <- doVarLayout (varEndBytes vL + 1) v
          return $ vL' : vL : rest
    isStorage VarDef{varStorage = StorageStorage} = True
    isStorage _ = False

doVarLayout :: StorageBytes -> SolidityBasicType -> LayoutReader SolidityVarLayout
doVarLayout lastOffEnd varT = do
  tUsed <- usedBytes varT
  let startBytes = nextLayoutStart lastOffEnd tUsed
  return $ VarLayout {
    varStartBytes = startBytes,
    varEndBytes = startBytes + tUsed - 1
    }

usedBytes :: SolidityBasicType -> LayoutReader StorageBytes
usedBytes t = case t of
  Boolean -> return 1
  Address -> return addressBytes
  SignedInt b -> return b
  UnsignedInt b -> return b
  FixedBytes b -> return b
  DynamicBytes -> return keyBytes
  String -> return keyBytes
  FixedArray typ l -> do
    elemSize <- usedBytes typ
    let
      (newEach, numKeys) =
        if elemSize <= 32
        then (32 `quot` elemSize,
              l `quot` newEach + (if l `rem` newEach == 0 then 0 else 1))
        else (1, l * (elemSize `quot` 32)) -- always have rem = 0
    return $ keyBytes * numKeys
  DynamicArray _ -> return keyBytes
  Mapping _ _ -> return keyBytes
  Typedef typeID -> typeUsedBytes <$> findTypedef typeID

findTypedef :: DeclID -> LayoutReader SolidityTypeLayout
findTypedef typeID = do
  contractsL <- ask
  let typesL = Map.foldr Map.union Map.empty $ Map.map typesLayout contractsL
  return $ (\f k m def -> f def k m) 
    Map.findWithDefault typeID typesL $
    Map.findWithDefault theError (declContract typeID) contractsL `seq`
    contractTLayout addressBytes

  where 
    theError = error $ "Couldn't find type " ++ declName typeID ++
                       " in contract " ++ declContract typeID ++
                       " nor as a contract type."

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

