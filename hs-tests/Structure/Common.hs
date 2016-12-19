{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Structure.Common where

import Data.Bifunctor
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
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

fileHasContracts :: FileName -> FileContractsStructure -> [ContractName] -> Assertion
fileHasContracts fileName contracts = mapM_ (fileHasContract fileName contracts) 

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
--contractHasEvent = contractHas contractEvents eventError

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
--eventDefnIs = defnIs contractEvents eventError

