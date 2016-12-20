module Structure.Layout.Storage (test) where

import qualified Data.Map as Map
import Numeric.Natural

import Blockchain.Ethereum.Solidity
import Structure.Common
import Test.Combinators
import Test.Common

test :: TestTree
test = doTests "storage" structureTest [
  boolFencepost,
  singleKeyBasicPacking,
  multiKeyBasicPacking,
  singleKeyArrayPacking,
  multiKeyArrayPacking,
  singleKeyStructPacking,
  multiKeyStructPacking
  ]

boolFencepost :: StructureTestInput
boolFencepost = layoutStorageTest "boolFencepost"  ["bool"] [WithPos 0 0]

singleKeyBasicPacking :: StructureTestInput
singleKeyBasicPacking = 
  layoutStorageTest "singleKeyBasicPacking" 
    [
      "bool",
      "address",
      "bytes3",
      "int48"
    ]
    [
      WithPos 0 0,
      WithPos 1 20,
      WithPos 21 23,
      WithPos 24 29 
    ]

multiKeyBasicPacking :: StructureTestInput
multiKeyBasicPacking =
  layoutStorageTest "multiKeyBasicPacking" 
    [
      "bytes31",
      "int16",
      "int16[3]",
      "bool",
      "address"
    ]
    [
      WithPos 0 30,
      WithPos 32 33,
      WithPos 64 95,
      WithPos 96 96,
      WithPos 97 116
    ]

singleKeyArrayPacking :: StructureTestInput
singleKeyArrayPacking =
  layoutStorageTest "singleKeyArrayPacking"  ["uint24[10]"] [WithPos 0 31]

multiKeyArrayPacking :: StructureTestInput
multiKeyArrayPacking =
  layoutStorageTest "multiKeyArrayPacking"  ["uint24[20]"] [WithPos 0 63]

singleKeyStructPacking :: StructureTestInput
singleKeyStructPacking = 
  structStorageTest "singleKeyStructPacking" 
    [
      "bool",
      "address",
      "bytes3",
      "int48"
    ]
    [
      WithPos 0 0 Boolean,
      WithPos 1 20 Address,
      WithPos 21 23 (FixedBytes 3),
      WithPos 24 29 (SignedInt 6)
    ]
    32

multiKeyStructPacking :: StructureTestInput
multiKeyStructPacking =
  structStorageTest "multiKeyStructPacking" 
    [
      "bytes31",
      "int16",
      "int16[3]",
      "bool",
      "address"
    ]

    [
      WithPos 0 30 (FixedBytes 31),
      WithPos 32 33 (SignedInt 2),
      WithPos 64 95 (FixedArray (SignedInt 2) 3),
      WithPos 96 96 Boolean,
      WithPos 97 116 Address
    ]
    128

layoutStorageTest :: String -> [String] -> [DeclID -> WithPos DeclID] -> StructureTestInput
layoutStorageTest name types posMakers = (name, sources, tester)
  where
    sources = Map.singleton name $
      contractDefn "C" $
        foldl (##) "" $ zipWith (\t i -> varDecl t (makeVarName i)) types [0..]
    tester contracts = contractHasAllStorageVars name contracts "C" $
      zipWith ($) posMakers $ zipWith makeDeclID types [0..]
    makeDeclID typ = DeclID cID . makeVarName
    makeVarName i = "f" ++ show i
    cID = ContractID name "C"

structStorageTest :: String -> [String] -> [WithPos BasicType] -> Natural -> StructureTestInput
structStorageTest name types fields size = (name, sources, tester)
  where
    sources = Map.singleton name $
      contractDefn "C" $
        structDefn "S" types ##
        varDecl "S" "x"
    tester contracts = 
      contractHasAllStorageVars name contracts "C" 
        [WithPos 0 (size - 1) $ DeclID cID "x"] >>
      typeDefnIs name contracts "C" "S" 
        (Struct $ WithSize size $ Map.fromList $
          zip (map ('f' : ) $ map show [0..]) fields)
    cID = ContractID name "C"
