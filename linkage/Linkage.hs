-- |
-- Module: Linkage
-- Description: Function to figure out the nature of linkage references
--   once inheritance and imports are finally done.
-- Maintainer: Ryan Reich <ryan@blockapps.net>
module Linkage (doLinkage) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import Control.Monad
import Data.Maybe

import DAG
import SolidityTypes

-- | Determines for each linkage what its exact nature is, and validates
-- the DAG of library references.
doLinkage :: ContractsByID 'AfterInheritance -> ContractsByID 'AfterLinkage
doLinkage contracts = either (error "Library missing or cycle detected") id $ do
  let result = Map.map (doContractLinkage contracts) contracts
  validateLibraries result 
  return result

validateLibraries :: ContractsByID 'AfterLinkage -> Either (DAGError ContractID) ()
validateLibraries contracts = 
  checkDAG $ Map.map (librariesLinkage . contractLinkage) contracts

doContractLinkage :: ContractsByID 'AfterInheritance -> Contract 'AfterInheritance ->
                     Contract 'AfterLinkage
doContractLinkage 
  contracts
  c@Contract{
    contractLinkage, contractTypes, contractStorageVars, contractBases
    } =
  c{contractLinkage =
    CompleteLinkage{
      typesLinkage = resolvedLinks,
      librariesLinkage = Map.foldr pushIfLibrary [] resolvedLinks
      },
    contractTypes, contractStorageVars, contractBases
    }

  where
    resolvedLinks = Map.map (resolveLink contracts c) contractLinkage
    pushIfLibrary :: DetailedLink 'Incomplete -> [ContractID] -> [ContractID]
    pushIfLibrary (LibraryLink DeclID{declContract}) = (declContract :)
    pushIfLibrary _ = id

resolveLink :: ContractsByID 'AfterInheritance -> Contract 'AfterInheritance ->
               RoughLink -> DetailedLink 'Incomplete
resolveLink contracts contract (UnqualifiedLink name) = 
  tryPlainType $ tryContract theError
  where
    tryPlainType :: DetailedLink 'Incomplete -> DetailedLink 'Incomplete
    tryPlainType x = fromMaybe x $ do
      dID <- findTypeIDByName name contract
      return $ PlainLink dID

    tryContract :: DetailedLink 'Incomplete -> DetailedLink 'Incomplete
    tryContract x = fromMaybe x $ do
      guard $ cID `Map.member` contracts
      return $ ContractLink cID

    cID = thisCID{contractName = name}
    thisCID = contractID contract
    theError = error $
      "Name " ++ name ++
      " does not refer to a contract in file " ++ contractFile thisCID ++ 
      " or to a type in contract " ++ contractName thisCID ++
      " in that file"

resolveLink contracts contract QualifiedLink{linkQualifier, linkName} =
  tryInherited $ tryLibrary $ tryContract theError
  where
    tryInherited :: DetailedLink 'Incomplete -> DetailedLink 'Incomplete
    tryInherited x = fromMaybe x $ do
      let dID = DeclID baseCID linkName 
      guard $ declContract dID `elem` directBases (contractBases contract)
      guard $ dID `Map.member` byID (contractTypes contract)
      return $ InheritedLink dID

    tryLibrary :: DetailedLink 'Incomplete -> DetailedLink 'Incomplete
    tryLibrary x = fromMaybe x $ do
      libC <- Map.lookup baseCID contracts
      guard $ contractIsLibrary libC
      dID <- findTypeIDByName linkName libC
      return $ LibraryLink dID

    tryContract :: DetailedLink 'Incomplete -> DetailedLink 'Incomplete
    tryContract x = fromMaybe x $ do
      guard $ cID `Map.member` contracts
      return $ ContractLink cID

    cID = thisCID{contractName = qualifiedName}
    baseCID = thisCID{contractName = linkQualifier}
    thisCID = contractID contract

    qualifiedName = linkQualifier ++ "." ++ linkName
    theError = error $
      "The qualified name " ++ qualifiedName ++
      "does not refer visible type of a direct base contract " ++
      "of the contract " ++ contractName thisCID ++
      "or to a visible type of a library in file " ++ contractFile thisCID ++
      "or to a contract imported into that file"

findTypeIDByName :: Identifier -> Contract s -> Maybe DeclID
findTypeIDByName name contract = Map.lookup name $ byName $ contractTypes contract

contractID :: Contract 'AfterInheritance -> ContractID
contractID c = NonEmpty.head $ allBases $ contractBases c

