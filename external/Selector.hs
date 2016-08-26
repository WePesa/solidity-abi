{-# LANGUAGE NamedRecordPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Selector (selector) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Text.PrettyPrint

import qualified Crypto.Hash.SHA3 as SHA3 (hash)
import Numeric

import SolidityTypes
import LayoutTypes

selector :: Map DeclID (SolidityNewType WithPos) -> DeclID ->
            Identifier -> SolidityTuple -> String
selector allTypesL dID name args = hash4 $ signature allTypesL dID name args 
  where
    hash4 bs = concatMap toHex $ BS.unpack $ BS.take 4 $ SHA3.hash 256 bs
    toHex = zeroPad . flip showHex ""
    zeroPad [c] = ['0',c]
    zeroPad x = x

signature :: Map DeclID (SolidityNewType WithPos) -> DeclID ->
             Identifier -> SolidityTuple -> ByteString
signature allTypesL dID name args =
  encodeUtf8 $ T.pack $ name ++ sigArgTypes allTypesL dID args

sigArgTypes :: Map DeclID (SolidityNewType WithPos) -> DeclID -> SolidityTuple -> String
sigArgTypes allTypesL dID (TupleValue args) =
  show $ parens $ hcat $ punctuate (text ",") $
  map (sig allTypesL dID . varType) args

sig :: Map DeclID (SolidityNewType WithPos) -> DeclID -> SolidityBasicType -> Doc
sig _ _ Boolean = text "bool"
sig _ _ Address = text "address"
sig _ _ (SignedInt s) = text "int" <> natural (s * 8)
sig _ _ (UnsignedInt s) = text "uint" <> natural (s * 8)
sig _ _ (FixedBytes s) = text "bytes" <> natural s
sig _ _ DynamicBytes = text "bytes"
sig _ _ String = text "string"
sig allTypesL dID (FixedArray t l) = (sig allTypesL dID t) <> text "[" <> natural l <> text "]"
sig allTypesL dID (DynamicArray t) = (sig allTypesL dID t) <> text "[]"
sig allTypesL dID (Mapping d c) =
  text "mapping" <+> (parens $ sig allTypesL dID d <+> text "=>" <+> sig allTypesL dID c)
sig allTypesL dID{declContract = cName} (Typedef typeID) = 
  case Map.lookup typeID allTypesL
    Nothing  -> sig allTypesL Address -- Contract type
    Just Enum{names} -> sig allTypesL (UnsignedInt $ sizeOf names)
    Just _ -> text name

natural = integer . toInteger
