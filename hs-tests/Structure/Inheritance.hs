module Structure.Inheritance (test) where

import Test.Tasty

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

import Blockchain.Ethereum.Solidity
import Test.Combinators
import Structure.Common

test :: TestTree
test = testGroup "inheritance" $ map structureTest [
  c3Ordering,
  storageVariableLayout,
  variableNaming,
  functionNaming,
  typeNaming,
  inheritTypeVariableDeclaration,
  inheritTypeFunctionDefinition,
  aliasImportInheritance
  ]

c3Ordering :: StructureTestInput
c3Ordering = (name, sources, tester)
  where
    name = "c3Ordering"
    sources = Map.singleton name $
      contractDefnBases "C" "" ["B1", "B2"] ##
      contractDefnBases "B1" "" ["B3"] ##
      contractDefnBases "B2" "" ["B4"] ##
      contractDefnBases "B3" "" ["B4"] ##
      contractDefn "B4" ""
    tester contracts = 
      contractHasAllBases name contracts "C" $
      map (ContractID name) ["C", "B2", "B1", "B3", "B4"]

storageVariableLayout :: StructureTestInput
storageVariableLayout = (name, sources, tester)
  where
    name = "storageVariableLayout"
    sources = Map.singleton name $
      contractDefnBases "C" (varDecl "int" "x") ["D"] ##
      contractDefnBases "D" (varDecl "address" "y") ["E"] ##
      contractDefn "E" (varDecl "bool" "z")
    tester contracts = 
      contractHasAllStorageVars name contracts "C" [
        WithPos 0  0  (DeclID (cID "E") "z"),
        WithPos 1  20 (DeclID (cID "D") "y"),
        WithPos 32 63 (DeclID (cID "C") "x")
        ] 
    cID = ContractID name 

variableNaming :: StructureTestInput
variableNaming = namingTest "variableNaming" varDecl contractHasAllVars varDefnIs $
  \t -> VarDef{
    varVisibility = InternalVisible,
    varStorage = StorageStorage,
    varType = t
    }

functionNaming :: StructureTestInput
functionNaming = namingTest "functionNaming" (\t n -> functionDecl n [] [t]) contractHasAllFunctions functionDefnIs $
  \t -> FuncDef{
    funcVisibility = PublicVisible,
    funcHasCode = True,
    funcIsConstant = False,
    funcIsConstructor = False,
    funcArgType = TupleValue [],
    funcValueType = TupleValue [ArgDef "" t MemoryStorage]
    }

typeNaming :: StructureTestInput
typeNaming = namingTest "typeNaming" (\t n -> structDefn n [t]) contractHasAllTypes typeDefnIs $
  \t -> Struct $ WithSize 32 $ Map.singleton "f0" $ WithPos 0 0 t

inheritTypeVariableDeclaration :: StructureTestInput
inheritTypeVariableDeclaration = declarationTest "inheritTypeVariableDeclaration" varDecl varDefnIs $
  \t -> VarDef{
    varVisibility = InternalVisible,
    varStorage = StorageStorage,
    varType = t
    }

inheritTypeFunctionDefinition :: StructureTestInput
inheritTypeFunctionDefinition = declarationTest "inheritTypeFunctionDefinition" (\t n -> functionDecl n [] [t]) functionDefnIs $
  \t -> FuncDef{
    funcVisibility = PublicVisible,
    funcHasCode = True,
    funcIsConstant = False,
    funcIsConstructor = False,
    funcArgType = TupleValue [],
    funcValueType = TupleValue [ArgDef "" t MemoryStorage]
    }

inheritTypeTypeDefinition :: StructureTestInput
inheritTypeTypeDefinition = declarationTest "inheritTypeTypeDefinition" (\n t -> structDefn n [t]) typeDefnIs $
  \t -> Struct $ WithSize 32 $ Map.singleton "f0" $ WithPos 0 0 t

aliasImportInheritance :: StructureTestInput
aliasImportInheritance = (name, sources, tester)
  where
    name = "aliasImportInheritance" 
    sources = Map.fromList [
      (name, 
        importer "D" import1Name ## contractDefnBases "C" "" [aliaser "D"]),
      (import1Name, 
        importer "E" import2Name ## contractDefnBases "D" "" [aliaser "E"]),
      (import2Name, 
        contractDefn "E" "")
      ]
    import1Name = name ++ "_import1"
    import2Name = name ++ "_import2"
    importer cName fName = importFileES6Aliases fName [cName] [aliaser cName]
    tester contracts = fileHasContracts name contracts ["C", aliaser "D"]
    aliaser = (++ "_Imported")

namingTest name declMaker hasAllDecls declDefnIs defnMaker = (name, sources, tester)
  where
    sources = Map.singleton name $
      contractDefnBases "C" (declMaker "int8" "x" ## declMaker "int8" "y") ["D"] ##
      contractDefnBases "D" (declMaker "bool" "y" ## declMaker "int8" "z") ["E"] ##
      contractDefn      "E" (declMaker "bool" "z" ## declMaker "int8" "w")
    tester contracts = 
      hasAllDecls name contracts "C" ["x", "y", "z", "w"] >>
      (forM_ ["x", "y", "z", "w"] $ \i -> declDefnIs name contracts "C" i (defnMaker $ SignedInt 1))

declarationTest name declMaker defnIs defnMaker = (name, sources, tester)
  where
    sources = Map.singleton name $
      contractDefnBases "C" (declMaker "T" "x") ["D"] ##
      contractDefn "D" (structDefn "T" ["int"])
    tester contracts = defnIs name contracts "C" "x" (defnMaker $ LinkT $ LinkID (ContractID name "C") (UnqualifiedLink "T"))

