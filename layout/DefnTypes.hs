module DefnTypes where

import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Maybe

import ParserTypes

type IdentT a = Map Identifier a

data SolidityContractDef =
  ContractDef {
    objsDef :: SolidityObjsDef,
    typesDef :: SolidityTypesDef,
    inherits :: [(ContractName, SolidityContractDef)]
    } deriving (Show)
type SolidityContractsDef = IdentT SolidityContractDef
type SolidityTypesDef = IdentT SolidityNewType
type SolidityObjsDef = [SolidityObjDef]

instance Monoid SolidityContractDef where
  mappend (ContractDef o1 t1 i1) (ContractDef o2 t2 i2) =
    -- o2 o1 is important : objects of the base come before objects of derived
    ContractDef (List.unionBy ((==) `on` objName) o2 o1) (t1 `Map.union` t2) (i1 ++ i2)
  mempty = ContractDef [] Map.empty []

makeFilesDef :: Map FileName SolidityFile -> Map FileName SolidityContractsDef
makeFilesDef files = result
  where result = Map.map (makeContractsDef result) files

makeContractsDef :: Map FileName SolidityContractsDef -> SolidityFile -> SolidityContractsDef
makeContractsDef fileDefs (SolidityFile contracts imports) =
  Map.map finalize $ c3Linearized contractDefs importDefs
  where
    contractDefs = Map.fromList $ map contractToDef contracts
    contractToDef (Contract name objs types bases) =
      (name, ContractDef objs (makeTypesDef types) (map getContractDef bases))
    getContractDef (name, _) = (name, Map.findWithDefault (error $ "Couldn't find base contract named " ++ name) name allDefs)
    allDefs = importDefs `Map.union` contractDefs
    importDefs = getImportDefs fileDefs imports
    finalize (ContractDef objsD typesD bases) = 
      ContractDef objsD (typesD `Map.union` contractTypes') bases
    contractTypes' =
      makeTypesDef $ map (\(name, _) -> TypeDef name ContractT) $ Map.toList allDefs

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


