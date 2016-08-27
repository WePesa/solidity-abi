{-# LANGUAGE NamedFieldPuns #-}
module Layout (doContractLayouts) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.Reader

import LayoutTypes
import SolidityTypes
import DAG

doContractLayouts :: ContractsByName -> ContractsABIByName
doContractLayouts contracts = either (error "Library layout error") id $ do
  validateLibraries contracts
  return $ Map.map (doContractLayout $ Map.keys contracts) contracts

validateLibraries :: ContractsByName -> Either (DAGError ContractName) ()
validateLibraries contracts = 
  checkDAG $ Map.map (map declContract . contractLibraryTypes) contracts

doContractLayout :: [ContractName] -> SolidityContract -> SolidityContractABI
doContractLayout cNames contract =
  Contract{
    contractVars = contractVars contract,
    contractFuncs = contractFuncs contract,
    contractEvents = contractEvents contract,
    contractModifiers = contractModifiers contract,
    contractInherits = contractInherits contract,
    contractExternalNames = contractExternalNames contract,
    contractLibraryTypes = contractLibraryTypes contract,
    contractIsConcrete = contractIsConcrete contract,
    contractIsLibrary = contractIsLibrary contract,

    -- The only two that change
    contractTypes = typesL,
    contractStorageVars = varsL
    }

  where
    (typesL, varsL) = runLayoutReader $ do
      typesByName <- mapM doTypeLayout $ byName cTypes
      typesByID <- mapM doTypeLayout $ byID cTypes
      varsL <- doVarsLayout (byID $ contractVars contract) (contractStorageVars contract)
      return (
        DeclarationsBy {
          byName = typesByName,
          byID = typesByID
          }, 
        varsL)
    runLayoutReader = flip runReader (cNames, byID typesL)
    cTypes = contractTypes contract

type LayoutReader = Reader ([ContractName], Map DeclID SolidityNewTypeABI)

doTypeLayout :: SolidityNewType -> LayoutReader SolidityNewTypeABI
doTypeLayout Enum{names} = 
  return EnumPos{
    namesPos = WithSize{
      sizeOf = ceiling $ logBase (256::Double) $ fromIntegral $ length names,
      stored = names
      }
    }
doTypeLayout Struct{fields} = do
  fieldsLayout <- doVarTypesLayout $ map fieldType fields
  return StructPos{
    fieldsPos = WithSize{
      sizeOf = 1 + endPos (last fieldsLayout),
      stored = zipWith makeFieldDefL fields fieldsLayout
      }
    }

  where
    makeFieldDefL fD fDL@WithPos{startPos, endPos} = WithPos{startPos, endPos, stored = fD}

doVarsLayout :: Map DeclID SolidityVarDef -> [DeclID] -> LayoutReader [WithPos DeclID]
doVarsLayout varDefs varIDs = do
  varLayouts <- doVarTypesLayout $ map (varType . (varDefs Map.!)) varIDs
  return $ zipWith makeDeclIDL varIDs varLayouts 

  where
    makeDeclIDL vID WithPos{startPos, endPos} = WithPos{startPos, endPos, stored = vID}

doVarTypesLayout :: [SolidityBasicType] -> LayoutReader [WithPos SolidityBasicType]
doVarTypesLayout =
  -- vars are listed from high storage to low; we work from the right
  foldr (\v vLs -> makeNextVarL v vLs) (return []) 
  where
    makeNextVarL v vLsR = do
      vLs <- vLsR
      case vLs of
        [] -> sequence [doVarLayout 0 v]
        l@(vL : rest) -> do
          vL' <- doVarLayout (endPos vL + 1) v
          return $ vL' : l

doVarLayout :: StorageBytes -> SolidityBasicType -> LayoutReader (WithPos SolidityBasicType)
doVarLayout lastOffEnd varT = do
  tUsed <- usedBytes varT
  let startPos = nextLayoutStart lastOffEnd tUsed
  return WithPos{
    startPos,
    endPos = startPos + tUsed - 1,
    stored = varT
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
  Typedef typeID -> typeSize <$> findTypedef typeID -- this is defined in doTypesLayout

findTypedef :: DeclID -> LayoutReader SolidityNewTypeABI
findTypedef typeID = do
  (cNames, typesL) <- ask
  return $ (\f k m def -> f def k m) 
    Map.findWithDefault typeID typesL $
    if declContract typeID `elem` cNames
    then contractT
    else theError
    
  where 
    theError = error $ "Couldn't find type " ++ declName typeID ++
                       " in contract " ++ declContract typeID ++
                       " nor as a contract type."
    contractT = ContractTPos {
      contractTPos = WithPos {
        startPos = 0,
        endPos = addressBytes,
        stored = ()
        }
      }

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

