module Linkage (doLinkage) where

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad
import Data.Maybe

import SolidityTypes

doLinkage :: ContractsByID 'AfterInheritance -> ContractsByID 'AfterLinkage
doLinkage contracts = either (error "Library missing or cycle detected") id $ do
  validateLibraries contracts 
  return $ Map.map (doContractLinkage contracts) contracts

validateLibraries :: ContractsByID 'AfterInheritance -> Either (DAGError ContractName) ()
validateLibraries contracts = 
  checkDAG $ Map.map (librariesLinkage . contractLinkage) contracts

doContractLinkage :: ContractsByID 'AfterInheritance -> Contract 'AfterInheritance ->
                     Contract 'AfterLinkage
doContractLinkage contracts contract{contractLinkage} =
  contract{contractLinkage =
    CompleteLinkage{
      typesLinkage = resolvedLinks,
      librariesLinkage = Map.foldr pushIfLibrary [] resolvedLinks
      }
    }

  where
    resolvedLinks = Map.map (resolveLink contracts contract) contractLinkage,
    pushIfLibrary (LibraryLink declID{declContract} _) = (declContract :)
    pushIfLibrary _ = id

resolveLink :: ContractsByID 'AfterInheritance -> Contract 'AfterInheritance ->
               LinkT 'Incomplete -> LinkT 'Complete
resolveLink contracts contract (UnqualifiedLink name) = 
  tryPlainType $ tryContract $ theError
  where
    tryPlainType x = fromMaybe x $ do
      dID <- findTypeIDByName name contract
      return $ PlainLink dID
    tryContract x = fromMaybe x $ do
      guard $ cID `Map.member` contracts
      return $ ContractLink cID

    cID = thisCID{contractName = name}
    thisCID = contractID contract
    theError = error $
      "Name " ++ name ++
      " does not refer to a contract in file " ++ file ++ 
      " or to a type in contract " ++ contractName thisCID ++
      " in that file"

resolveLink contracts contract QualifiedLink{linkQualifier, linkName} =
  tryInherited $ tryLibrary $ tryContract $ theError
  where
    tryInherited x = fromMaybe x $ do
      let dID = DeclID baseCID linkName 
      guard $ declContract dID `elem` directBases (contractBases contract)
      guard $ dID `Map.member` byID (contractTypes contract)
      return $ InheritedLink dID
    tryLibrary x = fromMaybe x $ do
      libC <- Map.lookup baseCID contracts
      guard $ contractIsLibrary libC
      dID <- findTypeIDByName linkName libC
      return $ LibraryLink dID
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

findTypeIDByName :: Identifier -> Contract s -> Maybe (LinkT 'Complete)
findTypeIDByName name contract = Map.lookup name $ byName $ contractTypes contract

contractID :: Contract 'AfterInheritance -> ContractID
contractID c = NonEmpty.head $ allBases $ contractBases contract

