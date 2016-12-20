module Structure.Layout.Sizes (test) where

import qualified Data.Map as Map
import Numeric.Natural

import Blockchain.Ethereum.Solidity
import Structure.Common
import Test.Combinators
import Test.Common

test :: TestTree
test = doTests "sizes" structureTest [
  addressVarSize,
  boolVarSize,
  intVarSize,
  intSizedVarSize,
  uintVarSize,
  uintSizedVarSize,
  byteVarSize,
  bytesSizedVarSize,
  bytesVarSize,
  stringVarSize,
  arraySizedVarSize,
  arrayVarSize,
  mappingVarSize,
  contractVarSize,
  structVarSize,
  enumVarSize
  ]

addressVarSize :: StructureTestInput
addressVarSize = layoutSizeTest "addressVarSize" "address" 20

boolVarSize :: StructureTestInput
boolVarSize = layoutSizeTest "boolVarSize" "bool" 1

intVarSize :: StructureTestInput
intVarSize = layoutSizeTest "intVarSize" "int" 32

intSizedVarSize :: StructureTestInput
intSizedVarSize = layoutSizeTest "intSizedVarSize" "int64" 8

uintVarSize :: StructureTestInput
uintVarSize = layoutSizeTest "uintVarSize" "uint" 32

uintSizedVarSize :: StructureTestInput
uintSizedVarSize = layoutSizeTest "uintSizedVarSize" "uint64" 8

byteVarSize :: StructureTestInput
byteVarSize = layoutSizeTest "byteVarSize" "byte" 1

bytesSizedVarSize :: StructureTestInput
bytesSizedVarSize = layoutSizeTest "bytesSizedVarSize" "bytes8" 8

bytesVarSize :: StructureTestInput
bytesVarSize = layoutSizeTest "bytesVarSize" "bytes" 32

stringVarSize :: StructureTestInput
stringVarSize = layoutSizeTest "stringVarSize" "string" 32

arraySizedVarSize :: StructureTestInput
arraySizedVarSize = layoutSizeTest "arraySizedVarSize" "bytes10[4]" 64

arrayVarSize :: StructureTestInput
arrayVarSize = layoutSizeTest "arrayVarSize" "int[]" 32

mappingVarSize :: StructureTestInput
mappingVarSize = layoutSizeTest "mappingVarSize" "mapping(int => int)" 32

contractVarSize :: StructureTestInput
contractVarSize = (name, sources, tester)
  where
    name = "contractVarSize"
    sources = Map.singleton name $ 
      contractDefn "C" (varDecl "D" "x") ##
      contractDefn "D" ""
    tester contracts =
      contractHasAllStorageVars name contracts "C" [WithPos 0 19 $ dID]
    dID = DeclID cID "x"
    cID = ContractID name "C"

structVarSize :: StructureTestInput
structVarSize = (name, sources, tester)
  where
    name = "structVarSize"
    sources = Map.singleton name $ 
      contractDefn "C" $ 
        structDefn "S" ["address", "address"] ##
        varDecl "S" "x"
    tester contracts =
      contractHasAllStorageVars name contracts "C" [WithPos 0 63 $ dID]
    dID = DeclID cID "x"
    cID = ContractID name "C"

enumVarSize :: StructureTestInput
enumVarSize = (name, sources, tester)
  where
    name = "enumVarSize"
    sources = Map.singleton name $ 
      contractDefn "C" $
        enumDefn "E" (map (\i -> "E" ++ show i) [0..256]) ##
        varDecl "E" "x"
    tester contracts =
      contractHasAllStorageVars name contracts "C" [WithPos 0 1 $ dID]
    dID = DeclID cID "x"
    cID = ContractID name "C"

oneEnumVarSize :: StructureTestInput
oneEnumVarSize = (name, sources, tester)
  where
    name = "oneEnumVarSize"
    sources = Map.singleton name $ 
      contractDefn "C" $
        enumDefn "E" ["E0"] ##
        varDecl "E" "x"
    tester contracts =
      contractHasAllStorageVars name contracts "C" [WithPos 0 0 $ dID]
    dID = DeclID cID "x"
    cID = ContractID name "C"

layoutSizeTest :: String -> String -> Natural -> StructureTestInput
layoutSizeTest name typ size = (name, sources, tester)
  where
    sources = Map.singleton name $ contractDefn "C" $ varDecl typ "x"
    tester contracts =
      contractHasAllStorageVars name contracts "C" [WithPos 0 (size - 1) $ dID]
    dID = DeclID cID "x"
    cID = ContractID name "C"

