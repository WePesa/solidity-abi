{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JSON (jsonABI) where

import Control.Monad.Fix

import Data.Functor.Identity

import Data.Aeson hiding (String)
import qualified Data.Aeson as Aeson (Value(String))
import Data.Aeson.Types (Pair)
import Data.Bifunctor
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import Data.Maybe
import Data.String

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Text as Text

import Qualify
import Imports
import Libraries
import Layout
import Defn
import DefnTypes
import LayoutTypes
import ParserTypes
import Selector

instance ToJSON SolidityFile where
  toJSON f = either id id $ jsonABI "" (Map.singleton "" f)

convertImportError :: ImportError -> Value
convertImportError (ImportCycle fBase) =
  object [pair "importError" "importCycle", 
          pair "inFile" fBase]
convertImportError (MissingImport fBase fName) = 
  object [pair "importError" "missingImport",
          pair "missingImport" fName,
          pair "inFile" fBase]
convertImportError (MissingSymbol fBase symName fName) = 
  object [pair "importError" "missingSymbol", 
          pair "missingSymbol" symName, 
          pair "fileName" fName, 
          pair "inFile" fBase]
convertImportError (DuplicateSymbol fBase fName symName) = 
  object [pair "importError" "duplicateSymbol", 
          pair "fileName" fName, 
          pair "duplicateSymbol" symName, 
          pair "inFile" fBase]
convertImportError (DuplicateFile fName) = 
  object [pair "importError" "duplicateFile", 
          pair "fileName" "fName"]

convertLibraryError :: LibraryError -> Value
convertLibraryError = undefined

convertDefnError :: DefnError -> Value
convertDefnError = undefined

convertLayoutError :: LayoutError -> Value
convertLayoutError = undefined

convertQualifyError :: QualifyError a b c -> Value
convertQualifyError = undefined

data ContractABI =
  ContractABI {
    contractVarsABI :: Value,
    contractFuncsABI :: Value,
    contractTypesABI :: Map Identifier Value,
    contractLibraryTypesABI :: Map ContractName (Map Identifier Value),
    contractConstrABI :: Value,
    contractLibraryABI :: Bool
    }

instance ToJSON ContractABI where
  toJSON (ContractABI v f t tL c l) =
    object $ 
      (nonempty (pair "vars") v) ++
      (nonempty (pair "funcs") f) ++
      (nonempty (pair "types") $ toJSON t) ++
      (nonempty (pair "libraryTypes") $ toJSON tL) ++
      (nonempty (pair "constr") c) ++
      (nonempty (pair "library") $ toJSON l)

jsonABI :: FileName -> Map FileName SolidityFile -> Either Value Value
jsonABI fileName files = mdo
  files' <- return $ either (error "Import path error") id $ first convertImportError $ fixAllPaths files
  first convertImportError $ validateImports files'
  filesDef <- return $ makeFilesDef files'
  results <- return $ filesABI results filesDef files'
  return $ toJSON $ getResult fileName results

  where
    getResult name = Map.findWithDefault (error $ "file name " ++ show name ++ " not found in results") name

filesABI :: Map FileName (Map ContractName ContractABI) ->
            Map FileName SolidityContractsDef ->
            Map FileName SolidityFile ->
            (Map FileName (Map ContractName ContractABI))
filesABI fileABIs filesDef solFiles = Map.mapWithKey (doFileABI solFiles) filesDef
  where
    doFileABI filesM fName = fileABI fName fileABIs $ fileImports $ getFile fName filesM
    getFile name = Map.findWithDefault (error $ "file name " ++ show name ++ " not found in files'") name

fileABI :: FileName ->
           Map FileName (Map ContractName ContractABI) ->
           [(FileName, ImportAs)] -> SolidityContractsDef ->
           (Map ContractName ContractABI)
fileABI fileName fileABIs imports fileDef = runIdentity $ mdo
  importsABI <- return $ either (error "Import error") id $ getImportDefs fileName fileABIs imports
  fileLayout <- return $ makeFileLayout fileDef
  fileABI <- return $ contractsABI fileABI fileLayout fileDef
  return $ importsABI `Map.union` fileABI

contractsABI :: Map ContractName ContractABI ->
                SolidityFileLayout ->
                SolidityContractsDef ->
                (Map ContractName ContractABI)
contractsABI fileABI fileLayout fileDef = Map.mapWithKey (contractABI fileABI fileLayout) fileDef

contractABI :: Map ContractName ContractABI -> SolidityFileLayout -> ContractName -> SolidityContractDef -> ContractABI
contractABI fABI fL name (ContractDef objs types lTypes _ isL) = runIdentity $ do
  lTypesABI <- return $ libTypesABI fABI lTypes
  return $ ContractABI {
      contractVarsABI = varsABI (varsLayout contractLayout) objs,
      contractFuncsABI = funcsABI typesL objs,
      contractTypesABI = typesABI typesL types,
      contractLibraryTypesABI = lTypesABI,
      contractConstrABI = constrABI name objs,
      contractLibraryABI = isL
      }
  where
    typesL = typesLayout contractLayout
    contractLayout = getContract name fL
    getContract cName = Map.findWithDefault (error $ "contract name " ++ show cName ++ " not found in file layout") cName

varsABI :: SolidityVarsLayout -> [SolidityObjDef] -> Value
varsABI layout' objs = object $ catMaybes $ map (\o -> varABI layout' o) objs

funcsABI :: SolidityTypesLayout -> [SolidityObjDef] -> Value
funcsABI typesL objs = object $ catMaybes $ map (funcABI typesL) objs
              
typesABI :: SolidityTypesLayout -> SolidityTypesDef -> Map Identifier Value
typesABI layout' types =
  Map.mapMaybeWithKey (\k t -> typeABI (getType k layout') k t) types
  where getType name = Map.findWithDefault (error $ "contract name " ++ show name ++ " not found in layout'") name

libTypesABI :: Map ContractName ContractABI -> [(ContractName, [Identifier])] -> (Map ContractName (Map Identifier Value))
libTypesABI filesABI lTypes = either (error "Library import error") id $ first convertQualifyError $ getQualifiedNames qAs typesABI
  where
    qAs = map (second $ QualifySome . (map $ \x -> (x,x))) lTypes
    typesABI = Map.map contractTypesABI filesABI

constrABI :: Identifier -> [SolidityObjDef] -> Value
constrABI name objs = object $ maybe [] listABI argsM
  where
    argsM = getArgs =<< List.find isConstr objs
    isConstr ObjDef{objName = name', objValueType = TupleValue _, objArgType = TupleValue _}
      | name == name' = True
    isConstr _ = False
    getArgs ObjDef{objArgType = TupleValue args} = Just args
    getArgs _ = Nothing

listABI :: [SolidityObjDef] -> [Pair]
listABI objs = do
  (i, (oName, oABI)) <- zip [0::Integer ..] $ fromMaybe [] $ mapM objABI objs
  let realName = if null oName then "#" ++ show i else oName
  return $ pair realName $ object $ (pair "index" i) : oABI

varABI :: SolidityVarsLayout -> SolidityObjDef -> Maybe Pair
varABI layout' var = do
  (name, tABI) <- objABI var
  let getVar name = Map.findWithDefault (error $ "variable name " ++ name ++ " not found in layout'") name
      vB = varStartBytes $ getVar (objName var) layout'
  return $ pair name $ object $ pair "atBytes" (toInteger vB) : tABI

funcABI :: SolidityTypesLayout -> SolidityObjDef -> Maybe Pair
funcABI typesL ObjDef{objName = name, objValueType = TupleValue vals, objArgType = TupleValue args} =
  Just $ pair name $ object [
           pair "selector" $ selector typesL name args vals,
           lpair "args" args,
           lpair "vals" vals
           ]
funcABI _ _ = Nothing

typeABI :: SolidityTypeLayout -> Identifier -> SolidityNewType -> Maybe Value
typeABI (StructLayout fieldsL tB) name (Struct fields') =
  Just $ object [
    pair "type" "Struct",
    pair "bytes" $ toInteger tB,
    pair "fields" $ varsABI fieldsL fields'
    ]
typeABI (EnumLayout tB) name (Enum names') =
  Just $ object $ [
    pair "type" "Enum",
    pair "bytes" $ toInteger tB,
    pair "names" names'
    ]
typeABI _ _ _ = Nothing

objABI :: SolidityObjDef -> Maybe (String, [Pair])
objABI (ObjDef name (SingleValue t) NoValue _ _ StorageStorage) = Just (name, basicTypeABI t)
objABI _ = Nothing

basicTypeABI :: SolidityBasicType -> [Pair]
basicTypeABI Boolean = [pair "type" "Bool"]
basicTypeABI Address = [pair "type" "Address"]
basicTypeABI (SignedInt b) = [
  pair "type" "Int",
  pair "signed" True,
  pair "bytes" $ toInteger b]
basicTypeABI (UnsignedInt b) = [
  pair "type" "Int",
  pair "bytes" $ toInteger b
  ]
basicTypeABI (FixedBytes b) = [
  pair "type" "Bytes",
  pair "bytes" $ toInteger b
  ]
basicTypeABI DynamicBytes = [
  pair "type" "Bytes",
  pair "dynamic" True
  ]
basicTypeABI String = [
  pair "type" "String",
  pair "dynamic" True
  ]
basicTypeABI (FixedArray eT l) = [
  pair "type" "Array",
  pair "length" $ toInteger l,
  tpair "entry" eT
  ]
basicTypeABI (DynamicArray eT) = [
  pair "type" "Array",
  pair "dynamic" True,
  tpair "entry" eT
  ]
basicTypeABI (Mapping dT cT) = [
  pair "type" "Mapping",
  pair "dynamic" True,
  tpair "key" dT,
  tpair "value" cT
  ]
basicTypeABI (Typedef name libM) = [
  pair "typedef" name
  ] ++ maybe [] (\x -> [pair "library" x]) libM

pair :: (ToJSON a) => String -> a -> Pair
pair x y = (fromString x, toJSON y)

tpair :: String -> SolidityBasicType -> Pair
tpair x y = (fromString x, object $ basicTypeABI y)

lpair :: String -> [SolidityObjDef] -> Pair
lpair x y = (fromString x, object $ listABI y)

nonempty :: (Value -> Pair) -> Value -> [Pair]
nonempty f ob@(Object o) =
  if HashMap.null o
  then []
  else [f ob]
nonempty f ar@(Array a) =
  if Vector.null a
  then []
  else [f ar]
nonempty f st@(Aeson.String s) =
  if Text.null s
  then []
  else [f st]
nonempty f bl@(Bool b) =
  if b
  then [f bl]
  else []
nonempty _ Null = []
nonempty f x = [f x]

