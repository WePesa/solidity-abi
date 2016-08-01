{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JSON (jsonABI) where

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
convertDefnError :: DefnError -> Value
convertLayoutError :: LayoutError -> Value
convertQualifyError :: QualifyError -> Value

data ContractABI =
  ContractABI {
    contractVarsABI :: Value,
    contractFuncsABI :: Value,
    contractTypesABI :: Map Identifier Value,
    contractLibraryTypesABI :: Value,
    contractConstrABI :: Value,
    contractLibraryABI :: Bool
    }

instance ToJSON ContractABI where
  toJSON (ContractABI v f t tL c l) =
    object $ 
      (nonempty (pair "vars" v)) ++
      (nonempty (pair "funcs" f)) ++
      (nonempty (pair "types" t)) ++
      (nonempty (pair "libraryTypes" tL)) ++
      (nonempty (pair "constr" c)) ++
      (nonempty (pair "library" l))

jsonABI :: FileName -> Map FileName SolidityFile -> Either Value Value
jsonABI fileName files = do
  files' <- first convertImportError $ fixAllPaths files
  filesDef <- first convertDefnError $ makeFilesDef files'
  let
    results = Map.mapWithKey (doFileABI files') filesDef
    doFileABI filesM fName fileDef =
      filesABI fName results (fileImports $ getFile fName filesM) fileDef
      where getFile name = Map.findWithDefault (error $ "file name " ++ show name ++ " not found in files'") name
    getResult name = Map.findWithDefault (error $ "file name " ++ show name ++ " not found in results") name
  result <- getResult fileName results
  return $ toJSON result

filesABI :: FileName ->
            Map FileName (Either ImportError (Map ContractName Value)) ->
            [(FileName, ImportAs)] -> SolidityContractsDef ->
            Either ImportError (Map ContractName Value)
filesABI fileName fileABIEs imports fileDef = do
  importsABI <- first convertImportError $ getImportDefs fileName fileABIEs imports
  let
    fileLayout = makeContractsLayout fileDef
    fileABI = sequence $ Map.mapWithKey (contractABI fileABI fileLayout) fileDef
  return $ (first convertQualifyError fileABI) `Map.union` importsABI

contractABI :: Map ContractName Value -> SolidityFileLayout -> ContractName -> SolidityContractDef -> Either QualifyError ContractABI
contractABI fABI fL name (ContractDef objs types lTypes _ isLibrary) = do
  lTypesABI <- typesABI (typesLayout contractLayout) types)
  return $ ContractABI {
      contractVarsABI = varsABI (objsLayout contractLayout) objs,
      contractFuncsABI = funcsABI objs,
      contractTypesABI = typesABI (typesLayout contractLayout) types,
      contractLibraryTypesABI = libTypesABI fABI lTypes,
      contractConstrABI = constrABI name objs,
      contractLibraryABI = isLibrary
      }
  where
    contractLayout = getContract name fL
    getContract cName = Map.findWithDefault (error $ "contract name " ++ show cName ++ " not found in file layout") oName

varsABI :: SolidityVarsLayout -> [SolidityObjDef] -> Value
varsABI layout' objs = object $ catMaybes $ map (\o -> varABI layout' o) objs

funcsABI :: [SolidityObjDef] -> Value
funcsABI objs = object $ catMaybes $ map funcABI objs
              
typesABI :: SolidityTypesLayout -> SolidityTypesDef -> Value
typesABI layout' types =
  object $ Map.elems $ Map.mapMaybeWithKey (\k t -> typeABI (getType k layout') k t) types
  where getType name = Map.findWithDefault (error $ "contract name " ++ show name ++ " not found in layout'") name

libTypesABI :: Map ContractName Value -> [(ContractName, [Identifier])] -> Either QualifyError Value
libTypesABI filesABI lTypes = object $ getQualifiedNames qAs lTypesABI
  where
    qAs = map (second $ map $ \x -> (x,x)) lTypes
    lTYpesABI = Map.map contractLibraryTypesABI filesABI

constrABI :: Identifier -> [SolidityObjDef] -> Value
constrABI name objs = object $ maybe [] listABI argsM
  where
    argsM = getArgs =<< List.find isConstr objs
    isConstr (ObjDef name' (TupleValue _) (TupleValue _) _) | name == name' = True
    isConstr _ = False
    getArgs (ObjDef _ _ (TupleValue args) _) = Just args
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

funcABI :: SolidityObjDef -> Maybe Pair
funcABI (ObjDef name (TupleValue vals) (TupleValue args) _) =
  Just $ pair name $ object [
           pair "selector" $ selector name args vals,
           lpair "args" args,
           lpair "vals" vals
           ]
funcABI _ = Nothing

typeABI :: SolidityTypeLayout -> Identifier -> SolidityNewType -> Maybe Pair
typeABI (StructLayout fieldsL tB) name (Struct fields') =
  Just $ pair name $ object [
    pair "type" "Struct",
    pair "bytes" $ toInteger tB,
    pair "fields" $ varsABI fieldsL fields'
    ]
typeABI (EnumLayout tB) name (Enum names') =
  Just $ pair name $ object $ [
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
basicTypeABI (Typedef name) = [
  pair "typedef" name
  ]

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
nonempty _ Null = []
nonempty f x = [f x]

