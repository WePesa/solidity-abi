module Structure.BasicTypes (test, basicTypeTestInput) where

import qualified Data.Map as Map

import Blockchain.Ethereum.Solidity
import Structure.Common
import Test.Combinators
import Test.Common

test :: TestTree
test = doTests "basicTypes" structureTest [
  intVar, intSizedVar, uintVar, uintSizedVar,
  byteVar, bytesSizedVar, bytesVar,
  boolVar, addressVar, stringVar
  ]

intVar :: StructureTestInput
intVar = basicTypeTestInput "intVar" "int" (SignedInt 32)

intSizedVar :: StructureTestInput
intSizedVar = basicTypeTestInput "intSizedVar" "int64" (SignedInt 8)

uintVar :: StructureTestInput
uintVar = basicTypeTestInput "uintVar" "uint" (UnsignedInt 32)

uintSizedVar :: StructureTestInput
uintSizedVar = basicTypeTestInput "uintSizedVar" "uint160" (UnsignedInt 20)

byteVar :: StructureTestInput
byteVar = basicTypeTestInput "byteVar" "byte" (FixedBytes 1)

bytesSizedVar :: StructureTestInput
bytesSizedVar = basicTypeTestInput "bytesSizedVar" "bytes17" (FixedBytes 17)

bytesVar :: StructureTestInput
bytesVar = basicTypeTestInput "bytesVar" "bytes" DynamicBytes

boolVar :: StructureTestInput
boolVar = basicTypeTestInput "boolVar" "bool" Boolean

addressVar :: StructureTestInput
addressVar = basicTypeTestInput "addressVar" "address" Address

stringVar :: StructureTestInput
stringVar = basicTypeTestInput "stringVar" "string" String

basicTypeTestInput :: String -> String -> BasicType -> StructureTestInput
basicTypeTestInput name typeName t = (name, sources, tester) where
  sources = Map.singleton name source
  source = contractDefn name $ varDecl typeName "x"
  tester contracts = varDefnIs name contracts name "x" $
    VarDef{
      varVisibility = InternalVisible,
      varStorage = StorageStorage,
      varType = t
      }

