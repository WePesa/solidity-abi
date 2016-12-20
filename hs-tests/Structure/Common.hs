{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Structure.Common where

import Data.Bifunctor
import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Blockchain.Ethereum.Solidity
import Test.Combinators
import Test.Common
import Test.ErrorMessages

type FileVerifier = FileContractsStructure -> Assertion
type StructureTestInput = (String, Map String SourceCode, FileVerifier)

structureTest :: StructureTestInput -> TestTree
structureTest (name, sources, tester) = makeTest name tester $ return $ parseToStructure sources

fileHasContract :: FileName -> FileContractsStructure -> ContractName -> Assertion
fileHasContract fileName contracts cName =
  cName `Map.member` (contracts Map.! fileName)
  |! fileError fileName ## isMissingError (contractError cName)

fileContractIsLibrary :: FileName -> FileContractsStructure -> ContractName -> Assertion
fileContractIsLibrary fileName contracts cName = 
  fileHasContract fileName contracts cName >>
  contractIsLibrary (contracts Map.! fileName Map.! cName)
  |! fileError fileName ## wrongThingError ("contract" ## cName) "library"

fileHasContracts :: FileName -> FileContractsStructure -> [ContractName] -> Assertion
fileHasContracts fileName contracts cNames = 
  Set.fromList cNames == Set.fromList theCNames
  |! fileError fileName ## wrongThingError ("contracts" ## show theCNames) (show cNames)

  where theCNames = Map.keys $ contracts Map.! fileName

contractHas :: (ContractStructure -> DeclarationsBy a) ->
              (String -> String) -> 
              (FileName -> FileContractsStructure -> ContractName -> Identifier -> Assertion)
contractHas contractDecls errorMaker fileName contracts cName name =
  fileHasContract fileName contracts cName >>
  name `Map.member` declsByName 
  |! fileError fileName ## contractError cName ## isMissingError (errorMaker name)

  where 
    declsByName = byNameDirectly $ contractDecls $ contracts Map.! fileName Map.! cName

contractHasVar = contractHas contractVars variableError
contractHasType = contractHas contractTypes typeError
contractHasFunction = contractHas contractFuncs functionError
contractHasEvent = contractHas contractEvents eventError

contractHasBase :: FileName -> FileContractsStructure -> ContractName -> ContractID -> Assertion
contractHasBase fileName contracts cName bID =
  fileHasContract fileName contracts cName >>
  fileHasContract (contractFile bID) contracts (contractName bID) >>
  bID `elem` NonEmpty.toList theBases
  |! fileError fileName ## contractError cName ## isMissingError ("base" ## show bID)

  where theBases = allBases $ contractBases $ contracts Map.! fileName Map.! cName

contractHasLibrary :: FileName -> FileContractsStructure -> ContractName -> ContractID -> Assertion
contractHasLibrary fileName contracts cName libID =
  fileHasContract fileName contracts cName >>
  fileContractIsLibrary (contractFile libID) contracts (contractName libID) >>
  libID `elem` theLibraries
  |! fileError fileName ## contractError cName ## isMissingError ("base" ## show libID)

  where theLibraries = librariesLinkage $ contractLinkage $ contracts Map.! fileName Map.! cName

contractHasAll :: (ContractStructure -> DeclarationsBy a) ->
                 String -> 
                 (FileName -> FileContractsStructure -> ContractName -> [Identifier] -> Assertion)
contractHasAll contractDecls errorName fileName contracts cName idents =
  fileHasContract fileName contracts cName >>
  Set.fromList idents == Set.fromList theIdents
  |! fileError fileName ## contractError cName ## wrongThingError (errorName ## show theIdents) (show idents)

  where theIdents = Map.keys $ byNameDirectly $ contractDecls $ contracts Map.! fileName Map.! cName
  
contractHasAllVars = contractHasAll contractVars "variables"
contractHasAllTypes = contractHasAll contractTypes "types"
contractHasAllFunctions = contractHasAll contractFuncs "functions"
contractHasAllEvents = contractHasAll contractEvents "events"

contractHasAllBases :: FileName -> FileContractsStructure -> ContractName -> [ContractID] -> Assertion
contractHasAllBases fileName contracts cName bases =
  fileHasContract fileName contracts cName >>
  NonEmpty.fromList bases == theBases
  |! fileError fileName ## contractError cName ## wrongThingError ("bases" ## show (NonEmpty.toList theBases)) (show bases)

  where theBases = allBases $ contractBases $ contracts Map.! fileName Map.! cName

contractHasAllStorageVars :: FileName -> FileContractsStructure -> ContractName -> [WithPos DeclID] -> Assertion
contractHasAllStorageVars fileName contracts cName vars =
  fileHasContract fileName contracts cName >>
  vars == theVars
  |! fileError fileName ## contractError cName ## wrongThingError ("storage vars" ## show theVars) (show vars)

  where theVars = reverse $ contractStorageVars $ contracts Map.! fileName Map.! cName

contractHasAllLibraries :: FileName -> FileContractsStructure -> ContractName -> [ContractID] -> Assertion
contractHasAllLibraries fileName contracts cName libs =
  fileHasContract fileName contracts cName >>
  libsSet == theLibsSet
  |! fileError fileName ## contractError cName ## wrongThingError ("libraries" ## show theLibsOrd) (show libsOrd)

  where 
    libsSet = Set.fromList libs
    libsOrd = Set.toList libsSet
    theLibsSet = Set.fromList $ librariesLinkage $ contractLinkage $ contracts Map.! fileName Map.! cName
    theLibsOrd = Set.toList theLibsSet

contractHasPlainLink :: FileName -> FileContractsStructure -> ContractName -> WithPos DeclID -> Assertion
contractHasPlainLink fileName contracts cName pos =
  contractHasType fileName contracts cName tName >>
  theLink == link
  |! fileError fileName ## contractError cName ## wrongThingError ("link" ## show theLink) (show link)

  where
    theLink = typesLinkage (contractLinkage $ contracts Map.! fileName Map.! cName) Map.! lID
    link = PlainLink pos
    tName = declName $ stored pos
    lID = LinkID cID $ UnqualifiedLink tName
    cID = ContractID fileName cName

contractHasInheritedLink :: FileName -> FileContractsStructure -> ContractName -> ContractID -> Identifier -> (DeclID -> WithPos DeclID) -> Assertion
contractHasInheritedLink fileName contracts cName bID tName posMaker =
  contractHasBase fileName contracts cName bID >>
  theLink == link
  |! fileError fileName ## contractError cName ## wrongThingError ("link" ## show theLink) (show link)

  where 
    theLink = typesLinkage (contractLinkage $ contracts Map.! fileName Map.! cName) Map.! lID
    link = InheritedLink (posMaker dID)
    lID = LinkID cID $ QualifiedLink (contractName bID) tName
    dID = DeclID bID tName
    cID = ContractID fileName cName

contractHasLibraryLink :: FileName -> FileContractsStructure -> ContractName -> ContractID -> Identifier -> (DeclID -> WithPos DeclID) -> Assertion
contractHasLibraryLink fileName contracts cName libID tName posMaker =
  contractHasLibrary fileName contracts cName libID >>
  theLink == link
  |! fileError fileName ## contractError cName ## wrongThingError ("link" ## show theLink) (show link)

  where 
    theLink = typesLinkage (contractLinkage $ contracts Map.! fileName Map.! cName) Map.! lID
    link = LibraryLink (posMaker dID)
    lID = LinkID cID $ QualifiedLink (contractName libID) tName
    dID = DeclID libID tName
    cID = ContractID fileName cName

contractHasContractLink :: FileName -> FileContractsStructure -> ContractName -> ContractName -> Assertion
contractHasContractLink fileName contracts cName c2Name =
  fileHasContract fileName contracts cName >>
  fileHasContract fileName contracts c2Name >>
  theLink == link
  |! fileError fileName ## contractError cName ## wrongThingError ("link" ## show theLink) (show link)

  where 
    theLink = typesLinkage (contractLinkage $ contracts Map.! fileName Map.! cName) Map.! lID
    link = ContractLink c2ID :: DetailedLinkStructure
    lID = LinkID cID $ UnqualifiedLink c2Name
    cID = ContractID fileName cName
    c2ID = ContractID fileName c2Name

defnIs :: (Eq a, Show a) =>
         (ContractStructure -> DeclarationsBy a) ->
         (String -> String) ->
         (FileName -> FileContractsStructure -> ContractName -> Identifier -> a -> Assertion)
defnIs contractDecls errorMaker fileName contracts cName name defn =
  contractHas contractDecls errorMaker fileName contracts cName name >>
  defn == theDefn
  |! fileError fileName ## contractError cName ## errorMaker name ## theError

  where
    theDefn = declsByName Map.! name
    declsByName = byNameDirectly $ contractDecls $ contracts Map.! fileName Map.! cName
    theError = wrongThingError ("type" ## show theDefn) (show defn)

varDefnIs = defnIs contractVars variableError
typeDefnIs = defnIs contractTypes typeError
functionDefnIs = defnIs contractFuncs functionError
eventDefnIs = defnIs contractEvents eventError

