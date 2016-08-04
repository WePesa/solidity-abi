{-# LANGUAGE RecursiveDo #-}
module Defn (makeFilesDef, DefnError(..)) where

import Control.Monad.Fix

import Data.Functor.Identity

import Data.Bifunctor
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Maybe
import Data.Traversable

import Imports
import DefnTypes
import ParserTypes

data DefnError = DefnImportError ImportError | MissingBase ContractName ContractName

instance Monoid SolidityContractDef where
  mappend (ContractDef o1 t1 lt1 i1 l1) (ContractDef o2 t2 lt2 i2 _) =
    ContractDef {
      -- o2 o1 is important : objects of the base come before objects of derived
      objsDef = List.unionBy ((==) `on` objName) o2 o1,
      typesDef = t1 `Map.union` t2,
      libraryTypes = lt1 ++ lt2,
      inherits = i1 ++ i2,
      library = l1
      }
  mempty = ContractDef [] Map.empty [] [] False

makeFilesDef :: Map FileName SolidityFile -> Map FileName SolidityContractsDef
makeFilesDef files = runIdentity $ mdo
  return $ either (error "Import validation error") id $ first DefnImportError $ validateImports files
  resultPairs <- return $ makeLinearizedFiles (Map.map snd resultPairs) files
  return $ Map.map fst resultPairs

makeLinearizedFiles :: Map FileName SolidityContractsDef -> Map FileName SolidityFile ->
                       Map FileName (SolidityContractsDef, SolidityContractsDef)
makeLinearizedFiles filesDef files = Map.mapWithKey (makeLinearizedContracts filesDef) files

makeLinearizedContracts :: Map FileName SolidityContractsDef ->
                           FileName ->
                           SolidityFile -> 
                           (SolidityContractsDef, SolidityContractsDef)
makeLinearizedContracts fileDefs fileName (SolidityFile contracts imports) = runIdentity $ do
  importDefs <- return $ either (error "Import error") id $ getImportDefs fileName fileDefs imports
  contractsDef <- return $ makeContractsDef importDefs contracts
  let
    allContractNames = Map.keys importDefs ++ map contractName contracts
    contractTypes = makeTypesDef $ map (flip TypeDef ContractT) allContractNames
    addContractTypes contract = contract{typesDef = typesDef contract `Map.union` contractTypes}
    result = Map.map addContractTypes $ c3Linearized contractsDef importDefs
  return (result, result `Map.union` importDefs)

makeContractsDef :: SolidityContractsDef -> [SolidityContract] -> SolidityContractsDef
makeContractsDef importDefs contracts = runIdentity $ mdo
  let contractDefsTrans = importDefs `Map.union` contractDefs
  contractDefs <- return $ Map.fromList $ makeContractsAssoc contractDefsTrans contracts
  return contractDefs

makeContractsAssoc :: SolidityContractsDef -> [SolidityContract] -> [(ContractName, SolidityContractDef)]
makeContractsAssoc allDefs contracts = map (makeContractDef allDefs) contracts

makeContractDef :: SolidityContractsDef -> SolidityContract -> (ContractName, SolidityContractDef)
makeContractDef allDefs (Contract name objs types lTypes bases isL) = runIdentity $ do
  baseDefs <- return $ either (error $ "Missing base error") id $ mapM getContractDef bases
  return $ (name, ContractDef {
    objsDef = objs,
    typesDef = makeTypesDef types,
    libraryTypes = librariesM,
    inherits = baseDefs,
    library = isL
    })

  where
    librariesM = map collect $ List.groupBy ((==) `on` fst) lTypes
    collect pairs = (fst $ head pairs, map snd pairs)
    getContractDef (bname, _) = do
      baseDef <- Map.findWithDefault (Left $ MissingBase name bname) bname $ Map.map Right allDefs
      return (bname, baseDef)

makeTypesDef :: [SolidityTypeDef] -> SolidityTypesDef
makeTypesDef types = Map.fromList $ map typeToTuple types
  where typeToTuple (TypeDef name decl) = (name, decl)

c3Linearized :: SolidityContractsDef -> SolidityContractsDef -> SolidityContractsDef
c3Linearized contracts imports = result
  where result = Map.map (c3Linearize $ imports `Map.union` result) contracts

c3Linearize :: SolidityContractsDef -> SolidityContractDef -> SolidityContractDef
c3Linearize c3Contracts contract =
  contract{inherits = []} <> c3Merge (map c3Lookup $ inherits contract)
  where c3Lookup (name, _) = (name, Map.findWithDefault (error $ "Couldn't find base contract named " ++ name ++ " while linearizing") name c3Contracts)

c3Merge :: [(ContractName, SolidityContractDef)] -> SolidityContractDef
c3Merge [] = mempty
c3Merge contracts = c3Head <> c3Merge c3Tail
  where
    (headName, c3Head) = contracts !! c3Index
    c3Tail = catMaybes $ do
      (name, contract) <- contracts
      let cPurge = filter (\(n', _) -> headName /= n') $ inherits contract
      if headName == name
        then return $ do
        (n', c') <- head' cPurge
        return (n', c'{inherits = tail' cPurge})        
        else return $ Just (name, contract{inherits = cPurge})
    c3Index = fromMaybe (error "Contract inheritance cannot be linearized") $
              List.findIndex isC3Head contracts
    isC3Head (name, _) =
      all (\names' -> not $ name `elem` names') $
      map (map fst . tail' . inherits . snd) contracts
    
    tail' [] = []
    tail' l = tail l
    head' [] = Nothing
    head' l = Just (head l)

