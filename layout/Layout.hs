-- |
-- Module: Layout
-- Description: Function for assigning storage locations to variables and
--   types in a parsed contract.
-- Maintainer: Ryan Reich <ryan@blockapps.net>
module Layout (doLayout) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans.Reader

import LayoutTypes
import SolidityTypes

-- | 'doLayout' analyzes the ordered list of global storage variables and
-- assigns them byte locations in the blockchain contract's storage.
--
-- Here are the rather picky rules for this:
-- 
-- * Every type has a size.  Most of the basic types have fixed sizes, in
-- many cases given in the declaration, but a few of them are "dynamic",
-- meaning that their actual values are stored in runtime-determined
-- storage locations.  Nonetheless, these also have sizes for the purpose
-- of this layout; their size is always 32 bytes.
-- * In general, variables are laid out contiguously in order of
-- declaration.  This includes fields of structs and elements of arrays.
-- * The first exception to this is that no variable may cross a 32-byte
-- boundary.  If it would, it gets moved to the next 32-byte slot.
-- * The second exception to this is that struct and array variables always
-- start and end in their own 32-byte slot.  That is, they start on
-- a 32-byte boundary and the following variable does as well.
doLayout :: ContractsByID 'AfterLinkage -> ContractsByID 'AfterLayout
doLayout contracts = result
  where result = Map.map (doContractLayout result) contracts

doContractLayout :: ContractsByID 'AfterLayout -> Contract 'AfterLinkage ->
                    Contract 'AfterLayout
doContractLayout
  contracts 
  c@Contract{
    contractVars, contractTypes, contractStorageVars, contractBases,
    contractLinkage = contractLinkage@CompleteLinkage{typesLinkage}
    } =
  c{
    contractTypes = contractTypes{byID = typesL},
    contractStorageVars = varsL,
    contractLinkage = contractLinkage{typesLinkage = sizedLinkage},
    contractBases
   }

  where
    sizedLinkage = Map.map (addLinkSize contracts typesL) typesLinkage
    (typesL, varsL) = runLayoutReader $ do
      typesL' <- mapM doTypeLayout cTypes
      varsL' <- doVarsLayout (byID contractVars) contractStorageVars
      return (typesL', varsL')
    runLayoutReader = 
      flip runReader (contracts, typesL, typesLinkage)
    cTypes = byID contractTypes

addLinkSize :: ContractsByID 'AfterLayout -> Map DeclID (NewType 'AfterLayout) ->
               TypeLink 'AfterLinkage -> TypeLink 'AfterLayout
addLinkSize contracts types link = case link of
  PlainLink dID -> 
    PlainLink WithSize{sizeOf = typeSize $ types Map.! dID, stored = dID}
  InheritedLink dID -> 
    InheritedLink WithSize{sizeOf = typeSize $ types Map.! dID, stored = dID}
  LibraryLink dID@DeclID{declContract = cID, declName} ->
    let 
      libTypes = contractTypes $ contracts Map.! cID
      libType = byID libTypes Map.! (byName libTypes Map.! declName)
    in LibraryLink WithSize{sizeOf = typeSize libType, stored = dID}
  ContractLink cID -> 
    ContractLink cID

type LayoutReader =
  Reader (
    ContractsByID 'AfterLayout, 
    Map DeclID (NewType 'AfterLayout), 
    LinkageT 'AfterLinkage
    )

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
      stored = Map.fromList $ zipWith makeFieldDefL fields fieldsLayout
      }
    }

  where makeFieldDefL fD fDL = (fieldName fD, const (fieldType fD) <$> fDL)

doVarsLayout :: Map DeclID VarDef -> StorageVars 'AfterLinkage ->
                LayoutReader (StorageVars 'AfterLayout)
doVarsLayout varDefs varIDs = do
  varLayouts <- doVarTypesLayout $ map (varType . (varDefs Map.!)) varIDs
  return $ zipWith makePosID varIDs varLayouts 

  where makePosID vID varPos = const vID <$> varPos

doVarTypesLayout :: [BasicType] -> LayoutReader [WithPos ()]
doVarTypesLayout =
  -- vars are listed from high storage to low; we work from the right
  foldr makeNextVarL $ return []
  where
    makeNextVarL v vLsR = do
      vLs <- vLsR
      case vLs of
        [] -> sequence [doVarLayout 0 v]
        l@(vL : _) -> do
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
    (contracts, typesL, typeLinks) <- ask 
    return $ case typeLinks Map.! linkTo of
      ContractLink _ -> 
        addressBytes
      PlainLink dID -> 
        typeSize $ typesL Map.! dID
      InheritedLink dID -> 
        typeSize $ typesL Map.! dID
      LibraryLink dID@DeclID{declContract} ->
        typeSize $ byID (contractTypes $ contracts Map.! declContract) Map.! dID

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
  in  if startKey0 == endKey0
      then thisStart0
      else thisStart1

