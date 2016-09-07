module Parser.Common where

import Distribution.TestSuite

import Data.Bifunctor
import Data.List
import Data.Maybe
import qualified Data.Set as Set

import Blockchain.Ethereum.Solidity.Parse
import Test.Combinators
import Test.Common
import Test.ErrorMessages

type FileVerifier = SolidityFile -> Result
type ParserTestInput = (String, SourceCode, FileVerifier)

parserTest :: ParserTestInput -> Test
parserTest (name, source, tester) = makeTest name tester $ parserStage name source

parserStage :: String -> SourceCode -> TestM SolidityFile
parserStage name source = first show $ parseSolidity name source

fileHasContract :: FileName -> SolidityFile -> ContractName -> Result
fileHasContract fileName solFile cName =
  cName `elem` map contractName (fileContracts solFile)
  |! fileError fileName ## theError

  where theError = isMissingError $ contractError cName 

contractHasVar :: FileName -> SolidityFile -> ContractName -> Identifier -> Result
contractHasVar fileName solFile cName vName =
  fileHasContract fileName solFile cName <>
  vName `elem` cVars
  |! fileError fileName ## contractError cName ## theError

  where
    theError = isMissingError $ variableError vName
    c = fromJust $ find (\c -> contractName c == cName) $ fileContracts solFile
    cVars = map objName $ filter isVar $ contractObjs c
    isVar ObjDef{objArgType = NoValue, objValueType = (SingleValue _)} = True
    isVar _ = False

varTypeIs :: FileName -> SolidityFile -> ContractName -> Identifier -> SolidityBasicType ->
             Result
varTypeIs fileName solFile cName vName t =
  contractHasVar fileName solFile cName vName <>
  objValueType theObj == SingleValue t
  |! fileError fileName ## contractError cName ## variableError vName ## theError

  where
    theError = wrongThingError
      ("type" ## show (objValueType theObj))
      (show $ SingleValue t)
    theObj = fromJust $ find (\obj -> objName obj == vName) $ contractObjs c
    c = fromJust $ find (\c -> contractName c == cName) $ fileContracts solFile

contractHasType :: FileName -> SolidityFile -> ContractName -> Identifier -> Result
contractHasType fileName solFile cName tName =
  fileHasContract fileName solFile cName <>
  tName `elem` cTypes
  |! fileError fileName ## contractError cName ## theError

  where
    theError = isMissingError $ typeError tName
    c = fromJust $ find (\c -> contractName c == cName) $ fileContracts solFile
    cTypes = map typeName $ contractTypes c

typeDefnIs :: FileName -> SolidityFile -> ContractName -> Identifier -> SolidityNewType ->
              Result
typeDefnIs fileName solFile cName tName d =
  contractHasType cName solFile cName tName <>
  typeDecl theType == d
  |! fileError fileName ## contractError cName ## typeError tName ## theError

  where
    theError = wrongThingError
      ("new type" ## show (typeDecl theType))
      (show d)
    c = fromJust $ find (\c -> contractName c == cName) $ fileContracts solFile
    theType = fromJust $ find (\typ -> typeName typ == tName) $ contractTypes c

contractHasFunction :: FileName -> SolidityFile -> ContractName -> Identifier -> Result
contractHasFunction fileName solFile cName fName =
  fileHasContract fileName solFile cName <>
  fName `elem` cFuncs
  |! fileError fileName ## contractError cName ## theError

  where
    theError = isMissingError $ functionError fName
    c = fromJust $ find (\c -> contractName c == cName) $ fileContracts solFile
    cFuncs = map objName $ filter isFunc $ contractObjs c
    isFunc ObjDef{objArgType = TupleValue _, objValueType = TupleValue _} = True
    isFunc _ = False

functionSignatureIs :: FileName -> SolidityFile -> ContractName -> Identifier ->
                       [Identifier] -> [SolidityBasicType] -> 
                       [Identifier] -> [SolidityBasicType] -> Result
functionSignatureIs fileName solFile cName fName argNames argTypes valNames valTypes =
  contractHasFunction fileName solFile cName fName <>
  tupleTypes (objArgType theObj) == argTypes &&
  tupleTypes (objValueType theObj) == valTypes &&
  and (zipWith (==) argNames $ tupleNames $ objArgType theObj) &&
  and (zipWith (==) valNames $ tupleNames $ objValueType theObj)
  |! fileError fileName ## contractError cName ## functionError fName ## theError

  where
    theError = wrongThingError
      ("args" ## show (zipWith (,) argTypes argNames) ##
       "and values" ## show (zipWith (,) valTypes valNames))
      ((show $ tupleBoth $ objArgType theObj) ## 
       "and" ## (show $ tupleBoth $ objValueType theObj))
    c = fromJust $ find (\c -> contractName c == cName) $ fileContracts solFile
    theObj = fromJust $ find (\obj -> objName obj == fName) $ contractObjs c
    getTuple (TupleValue l) = l
    getTuple _ = []
    tupleNames = map objName . getTuple
    tupleTypes = map getType . getTuple
    tupleBoth x = zipWith (,) (tupleTypes x) (tupleNames x)
    getType ObjDef{objValueType = SingleValue t} = t
