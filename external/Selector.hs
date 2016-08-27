{-# LANGUAGE NamedFieldPuns #-}
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

selector :: Map DeclID SolidityNewTypeABI -> Identifier -> SolidityTuple -> String
selector allTypesL name args = hash4 $ signature allTypesL name args 
  where
    hash4 bs = concatMap toHex $ BS.unpack $ BS.take 4 $ SHA3.hash 256 bs
    toHex = zeroPad . flip showHex ""
    zeroPad [c] = ['0',c]
    zeroPad x = x

signature :: Map DeclID SolidityNewTypeABI -> Identifier -> SolidityTuple -> ByteString
signature allTypesL name args =
  encodeUtf8 $ T.pack $ name ++ sigArgTypes allTypesL args

sigArgTypes :: Map DeclID SolidityNewTypeABI -> SolidityTuple -> String
sigArgTypes allTypesL (TupleValue args) =
  show $ parens $ hcat $ punctuate (text ",") $
  map (sig allTypesL . argType) args

sig :: Map DeclID SolidityNewTypeABI -> SolidityBasicType -> Doc
sig _ Boolean = text "bool"
sig _ Address = text "address"
sig _ (SignedInt s) = text "int" <> natural (s * 8)
sig _ (UnsignedInt s) = text "uint" <> natural (s * 8)
sig _ (FixedBytes s) = text "bytes" <> natural s
sig _ DynamicBytes = text "bytes"
sig _ String = text "string"
sig allTypesL (FixedArray t l) = (sig allTypesL t) <> text "[" <> natural l <> text "]"
sig allTypesL (DynamicArray t) = (sig allTypesL t) <> text "[]"
sig allTypesL (Mapping d c) =
  text "mapping" <+> (parens $ sig allTypesL d <+> text "=>" <+> sig allTypesL c)
sig allTypesL (Typedef typeID) = 
  case Map.lookup typeID allTypesL of
    Just EnumPos{namesPos} -> sig allTypesL (UnsignedInt $ sizeOf namesPos)
    Just StructPos{} -> text $ declName typeID -- Not actually allowed in functions
    _ -> sig allTypesL Address -- Contract type

natural = integer . toInteger
