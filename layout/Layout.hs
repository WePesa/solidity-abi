{-# LANGUAGE NamedFieldPuns #-}
module Layout (doLayout) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import LayoutTypes
import SolidityTypes
import DAG

doLayout :: ContractsByID 'AfterLinkage -> ContractsbyID 'AfterLayout
doLayout contracts = result
  where result = Map.map (doContractLayout result) contracts

doContractLayout :: ContractsByID 'AfterLayout -> Contract 'AfterLinkage ->
                    Contract 'AfterLayout
doContractLayout contracts contract{contractLinkage = CompleteLinkage{typesLinkage}} =
  contract{
    contractTypes = typesL,
    contractStorageVars = varsL,
    contractLinkage = contractLinkage{typesLinkage = newLibLinks}
    }

  where
    ((typesL, varsL), newLibLinks) = runLayoutM $ do
      typesByID <- mapM doTypeLayout cTypes
      varsL <- doVarsLayout (byID $ contractVars contract) (contractStorageVars contract)
      return (cTypes{byID = typesByID}, varsL)
    runLayoutM = 
      flip runReader (contracts, byID typesL) . 
      flip runStateT (typesLinkage $ contractLinkage contract)
    cTypes = byID $ contractTypes contract

type LayoutM = StateT (LinkageT 'AfterLayout) LayoutReader
type LayoutReader = Reader (ContractsByID 'AfterLayout, Map DeclID (NewType 'AfterLayout))

doTypeLayout :: NewType 'AfterLinkage -> LayoutM (NewType 'AfterLayout)
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
      stored = Map.fromList $ zipWith makeFieldDefL fields fieldsLayout
      }
    }

  where makeFieldDefL fD fDL = (fieldName fd, const (fieldType fD) <$> fDL)

doVarsLayout :: Map DeclID VarDef -> StorageVars 'AfterLinkage ->
                LayoutM (StorageVars 'AfterLayout)
doVarsLayout varDefs varIDs = do
  varLayouts <- doVarTypesLayout $ map (varType . (varDefs Map.!)) varIDs
  return $ zipWith makePosID varIDs varLayouts 

  where makePosID vID varPos = const vID <$> varPos

doVarTypesLayout :: [BasicType] -> LayoutM [WithPos ()]
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

doVarLayout :: StorageBytes -> BasicType -> LayoutM (WithPos ())
doVarLayout lastOffEnd varT = do
  tUsed <- usedBytes varT
  let startPos = nextLayoutStart lastOffEnd tUsed
  return WithPos{
    startPos,
    endPos = startPos + tUsed - 1,
    stored = ()
    }

usedBytes :: BasicType -> LayoutM StorageBytes
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
    (contracts, typesL) <- lift ask 
    typeLinks <- get
    let tLink = typeLinks Map.! linkTo
    case tLink of
      ContractLink _ -> return addressBytes
      _ -> case tLink of
        PlainLink dID -> return $ typeSize $ typesL Map.! dID
        InheritedLink dID -> return $ typeSize $ typesL Map.! dID
        LibraryLink (LibLinkID dID{declContract}) -> do
          let tSize = typeSize $ (contracts Map.! declContract) Map.! dID
          put $ Map.insert linkTo WithSize{sizeOf = tSize, stored = dID}
          return tSize

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

