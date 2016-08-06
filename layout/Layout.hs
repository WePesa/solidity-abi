module Layout (
  doLayout,
  SolidityContractsLayout, SolidityTypesLayout,
  SolidityVarsLayout,
  SolidityContractLayout(..), SolidityTypeLayout(..),
  SolidityVarLayout(..),
  StorageBytes
  ) where

import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.Reader

import ParserTypes
import LayoutTypes
import DAG

type LayoutReader = Reader (ContractName, SolidityContractsLayout)

doLayout :: SolidityContracts -> SolidityContractsLayout
doLayout contracts = either (error "Library layout error") id $ do
  validateLibraries contracts
  let result = Map.mapWithKey (\n c -> runReader (doContractLayout c) (n,result)) contracts
  return result

validateLibraries :: SolidityContracts -> Either DAGError ()
validateLibraries contractDefs = checkDAG $ Map.map (map fst . libraryTypes) contractDefs

doContractLayout :: SolidityContract -> LayoutReader SolidityContractLayout
doContractLayout contract = do
  varsL <- doVarsLayout $ contractVars contract
  typesL <- doTypesLayout $ contractTypes contract
  return $ ContractLayout {
    varsLayout = varsL,
    typesLayout = typesL,
    layoutIsLibrary = contractIsLibrary contract
    }

doTypesLayout :: SolidityTypes -> LayoutReader SolidityTypesLayout
doTypesLayout types = sequence $ Map.map doTypeLayout types

doTypeLayout :: SolidityTypeDef -> LayoutReader SolidityTypeLayout
doTypeLayout (Enum names)  =
  return $ EnumLayout {
    typeUsedBytes = ceiling $ logBase (256::Double) $ fromIntegral $ length names
    }
doTypeLayout (Struct fields) = do
  fieldsLayout <- doVarsLayout fields,
  let lastEnd = varEndBytes $ fieldsLayout Map.! varName (last fields)
  return $ StructLayout {
    structFieldsLayout = fieldsLayout,
    typeUsedBytes = nextLayoutStart lastEnd keyBytes
    }

doVarsLayout :: [SolidityVarDef] -> LayoutReader SolidityVarsLayout
doVarsLayout contractsL vars = do
  varsL <- foldr (\v vLs -> makeNextVarL v vLs) (return []) $ filter isStorage vars
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

doVarLayout :: StorageBytes -> SolidityVarDef -> LayoutReader SolidityVarLayout
doVarLayout lastOffEnd var = do
  tUsed <- usedBytes contractsL $ varType var
  let startBytes = nextLayoutStart lastOffEnd tUsed
  return $ VarLayout {
    varStartBytes = startBytes,
    varEndBytes = startBytes + tUsed - 1
    }

usedBytes :: SolidityBasicType -> LayoutReader StorageBytes
usedBytes contractsL t = case t of
  Boolean -> return 1
  Address -> return addressBytes
  SignedInt b -> return b
  UnsignedInt b -> return b
  FixedBytes b -> return b
  DynamicBytes -> return keyBytes
  String -> return keyBytes
  FixedArray typ l -> do
    elemSize <- usedBytes contractsL typ
    return $ keyBytes * numKeys
    where
      (newEach, numKeys) =
        if elemSize <= 32
        then (32 `quot` elemSize,
              l `quot` newEach + (if l `rem` newEach == 0 then 0 else 1))
        else (1, l * (elemSize `quot` 32)) -- always have rem = 0
  DynamicArray _ -> return keyBytes
  Mapping _ _ -> return keyBytes
  Typedef name libM -> typeUsedBytes <$> findTypeDef name libM

findTypeDef :: Identifier -> Maybe ContractName -> LayoutReader SolidityTypeLayout
findTypeDef name libM = do
  (cName, contractsL) <- get
  return $ either id notFound $ do
    findOrContinue name contract
    lib <- maybe (Left notFound) Right libM
    library <- maybe (Left $ error $ "No such library: " ++ lib) Left $
               Map.lookup lib contractsL
    when (not $ layoutIsLibrary library) $ Left $ error $ "Not a library: " ++ lib
    findOrContinue name library

  where
    findOrContinue n c = maybe (Right ()) Left $ Map.lookup name $ contractTypes c
    notFound =
      error $ "Type " ++ name ++ " not found in contract " ++ cName ++
      maybe "" (" or in library or base contract " ++) libM

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

