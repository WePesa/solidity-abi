{-# LANGUAGE NamedFieldPuns #-}
module Layout (doLayout) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.Reader

import LayoutTypes
import SolidityTypes
import DAG

doLayout :: ContractsByID 'AfterLinkage -> ContractsbyID 'AfterLayout
doLayout contracts = Map.map (doContractLayout contracts) contracts

doContractLayout :: ContractsByID 'AfterLinkage -> Contract 'AfterLinkage ->
                    Contract 'AfterLayout
doContractLayout contracts contract =
  contract{
    contractTypes = typesL,
    contractStorageVars = varsL
    }

  where
    (typesL, varsL) = runLayoutReader $ do
      typesByID <- mapM doTypeLayout cTypes
      varsL <- doVarsLayout (byID $ contractVars contract) (contractStorageVars contract)
      return (cTypes{byID = typesByID}, varsL)
    runLayoutReader = flip runReader (contracts, byID typesL)
    cTypes = byID $ contractTypes contract

type LayoutReader = Reader (ContractsByID 'AfterLinkage, Map DeclID (NewType 'AfterLayout))

doTypeLayout :: NewType 'AfterLinkage -> LayoutReader (NewType 'AfterLayout)
doTypeLayout enum@Enum{names} = 
  return enum{
    names = WithSize{
      sizeOf = ceiling $ logBase (256::Double) $ fromIntegral $ length names,
      stored = names
      }
    }
doTypeLayout struct@Struct{fields} = do
  fieldsLayout <- doVarTypesLayout $ map fieldType fields
  return struct{
    fields = WithSize{
      sizeOf = 1 + endPos (head fieldsLayout), -- fields are reversed
      stored = zipWith makeFieldDefL fields fieldsLayout
      }
    }

  where
    makeFieldDefL fD fDL@WithPos{startPos, endPos} = WithPos{startPos, endPos, stored = fD}

doVarsLayout :: Map DeclID VarDef -> StorageVars 'AfterLinkage ->
                LayoutReader (StorageVars 'AfterLayout)
doVarsLayout varDefs varIDs = do
  varLayouts <- doVarTypesLayout $ map (varType . (varDefs Map.!)) varIDs
  return $ zipWith makeDeclIDL varIDs varLayouts 

  where
    makeDeclIDL vID WithPos{startPos, endPos} = WithPos{startPos, endPos, stored = vID}

doVarTypesLayout :: [BasicType] -> LayoutReader [WithPos ()]
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

doVarLayout :: StorageBytes -> BasicType -> LayoutReader (WithPos ())
doVarLayout lastOffEnd varT = do
  tUsed <- usedBytes varT
  let startPos = nextLayoutStart lastOffEnd tUsed
  return WithPos{
    startPos,
    endPos = startPos + tUsed - 1,
    stored = ()
    }

usedBytes :: BasicType -> LayoutReader StorageBytes
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
  LinkT{linkTo} -> do
    (contracts, typesL) <- ask 
    case linkTo of
      ContractLink _ -> addressBytes
      _ -> typeSize $ case linkTo of
        PlainLink dID -> typesL Map.! dID
        InheritedLink dID -> typesL Map.! dID
        LibraryLink dID{declContract} -> (contracts Map.! declContract) Map.! dID

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

