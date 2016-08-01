module Defn (makeFilesDef, DefnError(..)) where

import Data.Bifunctor
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Maybe
import Data.Traversable

import Imports
import ParserTypes

type DefnError = DefnImportError ImportError | MissingBase ContactName ContractName

instance Monoid SolidityContractDef where
  mappend (ContractDef o1 t1 lt1 i1) (ContractDef o2 t2 lt2 i2) =
    ContractDef {
      -- o2 o1 is important : objects of the base come before objects of derived
      objsDef = List.unionBy ((==) `on` objName) o2 o1,
      typesDef = t1 `Map.union` t2,
      libraryTypes = lt1 `Map.Union` lt2,
      inherits = i1 ++ i2
      }
  mempty = ContractDef [] Map.empty []

makeFilesDef :: Map FileName SolidityFile -> Either DefnError (Map FileName SolidityContractsDef)
makeFilesDef files = do
  first DefnImportError $ validateImports files
  let
    resultPairs = sequence $ Map.mapWithKey doContractsDef files
    resultTrans = Map.map snd <$> resultPairs
    doContractsDef fn sf = (\fds -> makeLinearizedContracts fds fn sf) =<< resultTrans
  Map.map fst <$> resultPairs

makeLinearizedContracts :: Map FileName SolidityContractsDef ->
                          FileName ->
                          SolidityFile -> 
                          Either DefnError (SolidityContractsDef, SolidityContractsDef)
makeLinearizedContracts fileDefs fileName (SolidityFile contracts imports) = do
  importDefs <- first DefnImportError $ getImportDefs fileName fileDefs imports
  (contractsDef, contractsDefTrans) <- makeContractsDef importDefs contracts
  let
    linearizedContracts = c3Linearized contractsDef importDefs
    contractTypes = makeTypesDef $ map (flip TypeDef ContractT) $ Map.keys contractDefsTrans
    result = Map.map addContractTypes linearizedContracts
  return $ (result, result `Map.union` importDefs)
  where
    addContractTypes contract = contract{typesDef = typesDef contract `Map.union` contractTypes}

makeContractsDef :: SolidityContractsDef -> [SolidityContract] -> Either DefnError (SolidityContractsDef, SolidityContractsDef)
makeContractsDef importDefs contracts = (contractDefs, contractDefsTrans)
  where
    contractDefs = Map.fromList <$> mapM (makeContractsDef contractDefsTrans) contracts
    contractDefsTrans = Map.union importDefs =<< contractDefs

makeContractDef :: SolidityContractsDef -> SolidityContract -> Either DefnError (ContractName, SolidityContractDef)
makeContractDef allDefs (Contract name objs types lTypes bases) = do
  baseDefs <- mapM getContractDef bases
  return $ (name, ContractDef {
    objsDef = objs,
    typesDef = makeTypesDef types,
    libraryTypesDef = librariesM,
    inherits = baseDefs
    })

  where
    librariesM = map collect $ List.groupBy ((==) `on` fst) lTypes
    collect pairs = (fst $ head pairs, map snd pairs)
    getContractDef (bname, _) = do
      baseDef <- Map.findWithDefault (Left $ MissingBase name bName) bname $ Map.map Right allDefs
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

