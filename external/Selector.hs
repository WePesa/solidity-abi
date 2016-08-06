{-# LANGUAGE FlexibleInstances #-}
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

import ParserTypes
import LayoutTypes

selector :: SolidityTypesLayout -> Map ContractName SolidityTypesLayout ->
            Identifier -> SolidityTuple -> String
selector typesL allTypesL name args = hash4 $ signature typesL allTypesL name args 
  where
    hash4 bs = concatMap toHex $ BS.unpack $ BS.take 4 $ SHA3.hash 256 bs
    toHex = zeroPad . flip showHex ""
    zeroPad [c] = ['0',c]
    zeroPad x = x

signature :: SolidityTypesLayout -> Map ContractName SolidityTypesLayout ->
             Identifier -> SolidityTuple -> ByteString
signature typesL allTypesL name args =
  encodeUtf8 $ T.pack $ name ++ prettyArgTypes typesL allTypesL args

prettyArgTypes :: SolidityTypesLayout -> Map ContractName SolidityTypesLayout ->
                  SolidityTuple -> String
prettyArgTypes typesL allTypesL (TupleValue args) =
  show $ parens $ hcat $ punctuate (text ",") $
  map (pretty typesL allTypesL . varType) args

pretty :: SolidityTypesLayout -> Map ContractName SolidityTypesLayout ->
          SolidityBasicType -> Doc
pretty _ _ Boolean = text "bool"
pretty _ _ Address = text "address"
pretty _ _ (SignedInt s) = text "int" <> natural (s * 8)
pretty _ _ (UnsignedInt s) = text "uint" <> natural (s * 8)
pretty _ _ (FixedBytes s) = text "bytes" <> natural s
pretty _ _ DynamicBytes = text "bytes"
pretty _ _ String = text "string"
pretty typesL allTypesL (FixedArray t l) = (pretty typesL allTypesL t) <> text "[" <> natural l <> text "]"
pretty typesL allTypesL (DynamicArray t) = (pretty typesL allTypesL t) <> text "[]"
pretty typesL allTypesL (Mapping d c) =
  text "mapping" <+> (parens $ pretty typesL allTypesL d <+> text "=>" <+> pretty typesL allTypesL c)
pretty typesL allTypesL (Typedef name libM) = 
  case maybe typesL (allTypesL Map.!) libM Map.! name of
    EnumLayout s -> pretty typesL allTypesL (UnsignedInt s)
    _ -> text name

natural = integer . toInteger
