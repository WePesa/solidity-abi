module Layout (doContractLayouts) where

import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.Reader

import SolidityTypes
import LayoutTypes
import DAG

type LayoutReader = Reader (ContractID, SolidityContractsLayout)

doContractLayouts :: ContractsByID -> SolidityContractsLayout
doContractLayouts contracts = either (error "Library layout error") id $ do
  validateLibraries contracts
  let result = Map.mapWithKey (\cID c -> runReader (doContractLayout c) (cID,result)) contracts
  return result

validateLibraries :: ContractsByID -> Either (DAGError ContractName) ()
validateLibraries contracts = 
  checkDAG $ Map.map (Map.keys . contractLibraryTypes) contracts

doContractLayout :: SolidityContract -> LayoutReader SolidityContractLayout
doContractLayout contract = do
  varsL <- doVarsLayout (declaredVars $ contractDeclarationsByID contract) (contractVars contract)
  typesL <- doTypesLayout $ contractTypes contract
  return $ ContractLayout {
    varsLayout = varsL,
    typesLayout = typesL,
    layoutIsLibrary = contractIsLibrary contract
    }

doTypesLayout :: SolidityTypes -> LayoutReader SolidityTypesLayout
doTypesLayout types = sequence $ Map.map (doTypeLayout . typeDecl) types

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

doVarsLayout :: [DeclarationID] -> Map DeclarationID SolidityVarDef ->
                LayoutReader SolidityVarsLayout
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
  Typedef name libM -> typeUsedBytes <$> findTypedef name libM

findTypedef :: Identifier -> Maybe ContractName -> LayoutReader SolidityTypeLayout
findTypedef name libM = do
  (cID, contractsL) <- ask
  let
    contractL = contractsL Map.! cID
    notFound =
      error $ "Type " ++ name ++
              " not a contract nor found in contract " ++ contractRealName cID ++
              maybe "" (" nor in library or base contract " ++) libM
  return $ either id id $ do
    findOrContinue name contractL
    if cID{realContractName = name} `elem` Map.keys contractsL
    then Left $ ContractTLayout addressBytes
    else Right()
    lib <- maybe notFound Right libM
    library <- maybe (error $ "No such library: " ++ lib) Right $
               Map.lookup cID{realContractName = lib} contractsL
    when (not $ layoutIsLibrary library) $ error $ "Not a library: " ++ lib
    findOrContinue name library
    notFound

  where
    findOrContinue n c = maybe (Right ()) Left $ Map.lookup name $ declaredTypes $ contractDeclarationsByName c

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

