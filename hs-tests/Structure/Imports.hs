module Structure.Imports (test) where

import Test.Tasty

import Data.Map (Map)
import qualified Data.Map as Map

import Test.ErrorMessages
import Test.Combinators
import Test.Common ((|!))
import Structure.Common

test :: TestTree
test = testGroup "imports" $ map structureTest [
  basicImport, qualifiedBasicImport,
  basicStarImport, qualifiedBasicStarImport,
  es6Import, es6AliasImport,
  transitiveImport, diamondImport,
  es6AliasImportInheritance
  ]

basicImport :: StructureTestInput
basicImport = importTest "basicImport" (justFile importFile) noAliaser

qualifiedBasicImport :: StructureTestInput
qualifiedBasicImport = 
  importTest "qualifiedBasicImport" (justFile2 importFileAs prefix) (dotAliaser prefix)
  where prefix = "Imported"

basicStarImport :: StructureTestInput
basicStarImport = importTest "basicStarImport" (justFile importStarFile) noAliaser

qualifiedBasicStarImport :: StructureTestInput
qualifiedBasicStarImport = 
  importTest "qualifiedBasicStarImport" 
    (justFile2 importStarFileAs prefix) 
    (dotAliaser prefix)
  where prefix = "Imported"

es6Import :: StructureTestInput
es6Import = 
  importTest "es6Import" (\cName fName -> importFileES6Aliases fName [cName] [""]) noAliaser

es6AliasImport :: StructureTestInput
es6AliasImport =
  importTest "es6AliasImport" 
    (\cName fName -> importFileES6Aliases fName [cName] [alias])
    (constAliaser alias)

  where alias = "Imported"

transitiveImport :: StructureTestInput
transitiveImport = (name, sources, tester)
  where
    name = "transitiveImport"
    sources = Map.fromList [
      (name, importFile "F1" ## contractDefn "C1" ""),
      ("F1", importFile "F2" ## contractDefn "C2" ""),
      ("F2", contractDefn "C3" "")
      ]
    tester contracts = fileHasContracts name contracts ["C1", "C2", "C3"]

diamondImport :: StructureTestInput
diamondImport = (name, sources, tester)
  where
    name = "diamondImport"
    names = ["diamondImport", "F1", "F2", "F12"]
    sources = Map.fromList [
      (name, importFile "F1" ## importFile "F2" ## contractDefn "C" ""),
      ("F1", importFileAs "F12" "F1" ## contractDefn "C1" ""),
      ("F2", importFileAs "F12" "F2" ## contractDefn "C2" ""),
      ("F12", contractDefn "C12" "")
      ]
    tester contracts = fileHasContracts name contracts
      ["C", "C1", "C2", dotAliaser "F1" "C12", dotAliaser "F2" "C12"]

es6AliasImportInheritance :: StructureTestInput
es6AliasImportInheritance =
  importTestInheritance "es6AliasImportInheritance" 
    (\cName fName -> importFileES6Aliases fName [cName] [alias])
    (constAliaser alias)

  where alias = "Imported"

importTestInheritance :: String -> (String -> String -> String) -> (String -> String) ->
                         StructureTestInput
importTestInheritance name importer aliaser = (name, sources, tester)
  where
    sources = Map.fromList [
      (name, importer "D" importName ## contractDefnBases "C" "" [aliaser "D"]),
      (importName, contractDefnBases "D" "" ["E"] ## contractDefn "E" "")
      ]
    tester contracts = fileHasContracts name contracts ["C", aliaser "D"]
    importName = name ++ "_import"

importTest :: String -> (String -> String -> String) -> (String -> String) -> StructureTestInput
importTest name importer aliaser = (name, sources, tester)
  where
    sources = Map.fromList [
      (name, importer "D" importName ## contractDefn "C" ""),
      (importName, contractDefn "D" "")
      ]
    tester contracts = fileHasContracts name contracts ["C", aliaser "D"]
    importName = name ++ "_import"

{-# ANN dotAliaser "HLint: ignore Redundant bracket" #-}
dotAliaser :: String -> (String -> String)
dotAliaser prefix s = prefix ++ "." ++ s

{-# ANN constAliaser "HLint: ignore Redundant bracket" #-}
constAliaser :: String -> (String -> String)
constAliaser = const 

noAliaser :: String -> String
noAliaser = id

{-# ANN justFile "HLint: ignore Redundant bracket" #-}
justFile :: (String -> String) -> (String -> String -> String)
justFile = const 

{-# ANN justFile2 "HLint: ignore Redundant bracket" #-}
justFile2 :: (String -> String -> String) -> String -> (String -> String -> String)
justFile2 importer prefix = const $ flip importer prefix
