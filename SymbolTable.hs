module SymbolTable (
  StorageReference(..), StorageLocation(..),
  SymbolTableRow(..), SymbolMetadata(..), SymbolType(..),
  initSymbolTableRow, makeSymbolTable, toHex
  ) where

import Blockchain.ExtWord (Word256)
import qualified Crypto.Hash.SHA3 as SHA3
import Data.Bifunctor
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe
import qualified Data.Map as Map hiding (Map)
import Data.Map (Map)
import Numeric

import Pretty
import ParserTypes

data StorageReference =
  NoReference | -- Fixed-size types
  UndeterminedReference | -- Mapping types: reference depends on key
  AddressReference String -- Dynamic arrays

data StorageLocation =
  StorageLocation {
    storageKey :: Integer,
    storageValOffset :: Integer, -- Variables are tightly packed
    dataReference :: StorageReference
    }

data SymbolMetadata =
  EnumMetadata { enumNames0 :: Map String Integer } |
  StructMetadata { fieldsTable :: Map String SymbolTableRow } |
  ArrayMetadata { elementStorage :: Maybe SymbolTableRow,
                  arrayLen :: Maybe Integer,
                  newKeyAfterEvery :: Integer } |
  MappingMetadata { valueStorage :: SymbolTableRow,
                    keyStorage :: SymbolTableRow } |
  FunctionMetadata { functionSelector :: String,  -- For message calls
                     functionSignature :: String
                   }

data SymbolType =
  VariableType { symbolVarType :: String, genericType :: String } |
  FunctionType { functionArgTypes :: [SymbolTableRow],
                 functionArgNames :: [String],
                 functionRet :: Maybe SymbolTableRow }

data SymbolTableRow =
  SymbolTableRow {
    symbolType :: SymbolType,
    storageLocation :: Maybe StorageLocation,
    storageSize :: Integer,
    symbolMetadata :: Maybe SymbolMetadata
    }

initSymbolTableRow :: Map String SymbolTableRow -> SoliditySymbol
                      -> (String, SymbolTableRow)
initSymbolTableRow decls sym@Function{ funcName=name, args=fArgs, returns=ret } =
  ( name,
    SymbolTableRow {
       symbolType = FunctionType {
          functionArgTypes = map snd rows,
          functionArgNames = map fst rows,
          functionRet =
            (\x -> snd $ makeVariableSymbolTable decls [ Variable "" x ] !! 0) <$> ret
          },
       storageSize = 0,
       storageLocation = undefined,
       symbolMetadata =
         Just $ FunctionMetadata {
           functionSelector =
              concatMap toHex' $ BS.unpack $ BS.take 4 $
              BS.fromStrict $ SHA3.hash 256 $ BS.toStrict $
              canonicalSignature sym,
           functionSignature =
             "function(" ++
             (concat $ intersperse "," $ map (show . pretty . varType) fArgs) ++
             ") returns (" ++ (maybe "" (show . pretty) ret) ++ ")"
           }
       }
  )
  where rows = makeVariableSymbolTable decls fArgs
        toHex' = zeroPad . toHex
        zeroPad [c] = ['0',c]
        zeroPad x = x
         
initSymbolTableRow decls Variable{ varName = name, varType = vType } =
  ( name,
    SymbolTableRow {
       symbolType = VariableType {
          symbolVarType = show $ pretty vType,
          genericType = symGenericType
          },
       storageLocation =
         Just $ StorageLocation {
           storageKey = 0,
           storageValOffset = 0,
           dataReference = symDefaultDataReference
           },
       storageSize = symSize,
       symbolMetadata = symMetadata
       }
  )
  where
    defaultDataReference = AddressReference $ getArrayReference 0
    (symGenericType, symSize, symMetadata, symDefaultDataReference) = case vType of
      Boolean -> ("Bool", 1, Nothing, NoReference)
      Address -> ("Address", 20, Nothing, NoReference)
      SignedInt b -> ("Int", b, Nothing, NoReference)
      UnsignedInt b -> ("Int", b, Nothing, NoReference)
      FixedBytes b -> ("Bytes", b, Nothing, NoReference)
      DynamicBytes -> ("Bytes", 32,
                       Just $ ArrayMetadata {
                         elementStorage = Nothing,
                         arrayLen = Nothing,
                         newKeyAfterEvery = 32
                         },
                       defaultDataReference)
      String -> ("String", 32, Just $ ArrayMetadata { elementStorage = Nothing,
                                            arrayLen = Nothing,
                                            newKeyAfterEvery = 32 },
                 defaultDataReference)
      -- SignedReal b _ -> (b, Nothing, NoReference)
      -- UnsignedReal b _ -> (b, Nothing, NoReference)
      FixedArray t l ->
        let elemRow = snd $ head $ makeVariableSymbolTable decls
                      [Variable { varName = "", varType = t }]
            elemSize = storageSize elemRow
            (newEach, numSlots) =
              if elemSize <= 32
              then (32 `quot` elemSize,
                    l `quot` newEach + (if l `rem` newEach == 0 then 0 else 1))
              else (1, l * (elemSize `quot` 32)) -- always have rem = 0
        in ("Array", 32 * numSlots, Just $
                          ArrayMetadata { elementStorage = Just elemRow,
                                          arrayLen = Just l,
                                          newKeyAfterEvery = newEach},
            NoReference)
      DynamicArray t ->
        let elemRow = snd $ head $ makeVariableSymbolTable decls
                      [Variable { varName = "", varType = t }]
            elemSize = storageSize elemRow
        in ("Array", 32, Just $ ArrayMetadata {
               elementStorage = Just elemRow{ storageLocation = Nothing },
               arrayLen = Nothing,
               newKeyAfterEvery = max 1 $ 32 `quot` elemSize },
            defaultDataReference)
      Mapping d t ->
        let valRow = snd $ head $ makeVariableSymbolTable decls
                     [Variable { varName = "", varType = t }]
            keyRow = snd $ head $ makeVariableSymbolTable decls
                     [Variable { varName = "", varType = d }]
        in ("Mapping", 32, Just $ MappingMetadata {
               valueStorage = valRow{ storageLocation = Nothing},
               keyStorage = keyRow{ storageLocation = Nothing} }, 
            UndeterminedReference) 
      Enum names' ->
        ("Enum", ceiling $ logBase (8 :: Double) $ fromIntegral $ length names',
         Just $ EnumMetadata { enumNames0 = Map.fromList $ zip names' [0 .. ] },
         NoReference)
      Struct fields' ->
        let fieldRows = makeVariableSymbolTable decls fields'
            numSlots =
              if null fieldRows
              then 0
              else 1 + (storageKey $ fromJust $ storageLocation $ snd $ last fieldRows)
        in ("Struct", 32 * numSlots,
            Just $ StructMetadata { fieldsTable = Map.fromList fieldRows },
            NoReference)
      ContractT -> ("Contract", 20, Nothing, NoReference);
      UserDefined name' ->
        let Just realTypeRow = Map.lookup name' decls
        in (genericType $ symbolType realTypeRow,
            storageSize realTypeRow, Nothing, NoReference)

makeVariableSymbolTable :: Map String SymbolTableRow -> [SoliditySymbol]
                           -> [(String, SymbolTableRow)]
makeVariableSymbolTable decls vars = fst $ makeSymbolTable decls vars

makeSymbolTable :: Map String SymbolTableRow -> [SoliditySymbol]
                   -> ([(String, SymbolTableRow)], [(String, SymbolTableRow)])
makeSymbolTable decls syms = bimap makeStorage (map noStorage) varsFuncs
  where
    varsFuncs = partition isVariable syms
    isVariable (Variable {}) = True
    isVariable _ = False

    noStorage sym = (name, row{ storageLocation = Nothing })
      where (name, row) = initSymbolTableRow decls sym

    makeStorage = scanl1 addStorage . map (initSymbolTableRow decls)
    addStorage (_,row) (name, row') =
      let
        Just rowStorage = storageLocation row
        Just rowStorage' = storageLocation row'
        dr0 = dataReference rowStorage'

        lastOff = storageValOffset rowStorage
        thisOff0 = lastOff + storageSize row
        nextOff0 = thisOff0 + storageSize row'
        lastKey = storageKey rowStorage
        (newKey, newOff) =
          if thisOff0 >= 32 || nextOff0 > 32
          then
            let rowIncr = max 1 $ storageSize row `quot` 32
            in (lastKey + rowIncr, 0)
          else (lastKey, thisOff0)
      in (name, row' {
        storageLocation = Just $
           StorageLocation {
             storageKey = newKey,
             storageValOffset = newOff,
             dataReference = case dr0 of
               AddressReference _ -> AddressReference $ getArrayReference newKey
               _ -> dr0
             }
        })

getArrayReference :: Integer -> String
getArrayReference key =
  toHex (decode $ BS.fromStrict $ SHA3.hash 256 $ BS.toStrict $
  BS.reverse $ BS.take 32 $ BS.append (BS.reverse $ encode key) zeros :: Word256)
  where zeros = BS.pack $ repeat 0

toHex :: (Integral a, Show a) => a -> String
toHex = flip showHex ""
