{-# LANGUAGE RecursiveDo #-}
module Inheritance (doInheritance) where

import Control.Monad.Fix

import Data.Functor.Identity

import Data.Bifunctor
import Data.Function
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Maybe
import Data.Traversable

import ParserTypes

newtype Inherit = Inherit { fromInherit :: SolidityContract }

instance Monoid Inherit where
  -- c1 is derived from c2
  mappend (Inherit c1) (Inherit c2) =
    -- TODO: qualify the names of imported vars, funcs, events, modifiers
    Inherit $ Contract {
      -- objects are prepended to the list during parsing: later ones first
      contractVars = contractVars c1 ++ contractVars c2,
      contractFuncs = contractFuncs c1 `Map.union` contractFuncs c2,
      contractEvents = contractEvents c1 `Map.union` contractEvents c2,
      contractModifiers = contractModifiers c1 `Map.union` contractModifiers c2,
      -- derived contract types override base ones.  The originals are
      -- available in contractLibraryTypes.
      -- TODO: inherited types have their layout done twice like this
      contractTypes = contractTypes c1 `Map.union` contractTypes c2,
      contractLibraryTypes =
        typesLib `libUnion` contractLibraryTypes c1 `libUnion` contractLibraryTypes c2
        where
          libUnion = Map.unionWith Set.union
          typesLib = Map.singleton (contractName c2) $ Set.fromList $
                     map typeName $ contractTypes c2
      -- order of inheritance is right-to-left
      contractInherits = contractInherits c1 ++ contractInherits c2, 
      contractIsLibrary = contractIsLibrary c1
    }
  mempty = Inherit emptyContract

doInheritance :: SolidityContracts -> SolidityContracts
doInheritance contracts = Map.map fromInherit result
  where result = Map.map (c3Linearize result . Inherit) contracts

c3Linearize :: Map ContractName Inherit -> Inherit -> Inherit
c3Linearize c3Contracts contract =
  contract{contractInherits = []} <> c3Merge (map c3Lookup $ contractInherits contract)
  where c3Lookup (name, _) = (name, Map.findWithDefault (error $ "Couldn't find base contract named " ++ name ++ " while linearizing") name c3Contracts)

c3Merge :: [(ContractName, Inherit)] -> Inherit
c3Merge [] = mempty
c3Merge contracts = c3Head <> c3Merge c3Tail
  where
    (headName, c3Head) = contracts !! c3Index
    c3Tail = catMaybes $ do
      (name, contract) <- contracts
      let cPurge = filter (\(n', _) -> headName /= n') $ contractInherits contract
      if headName == name
        then return $ do
        (n', c') <- head' cPurge
        return (n', c'{contractInherits = tail' cPurge})        
        else return $ Just (name, contract{contractInherits = cPurge})
    c3Index = fromMaybe (error "Contract inheritance cannot be linearized") $
              List.findIndex isC3Head contracts
    isC3Head (name, _) =
      all (\names' -> not $ name `elem` names') $
      map (map fst . tail' . contractInherits . snd) contracts
    
    tail' [] = []
    tail' l = tail l
    head' [] = Nothing
    head' l = Just (head l)

