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

import SolidityTypes
import LayoutTypes

selector :: Map ContractID SolidityTypesLayout -> DeclarationID ->
            Identifier -> SolidityTuple -> String
selector allTypesL dID name args = hash4 $ signature allTypesL dID name args 
  where
    hash4 bs = concatMap toHex $ BS.unpack $ BS.take 4 $ SHA3.hash 256 bs
    toHex = zeroPad . flip showHex ""
    zeroPad [c] = ['0',c]
    zeroPad x = x

signature :: Map ContractID SolidityTypesLayout -> DeclarationID ->
             Identifier -> SolidityTuple -> ByteString
signature allTypesL dID name args =
  encodeUtf8 $ T.pack $ name ++ prettyArgTypes allTypesL dID args

prettyArgTypes ::  Map ContractID SolidityTypesLayout -> DeclarationID ->
                  SolidityTuple -> String
prettyArgTypes allTypesL dID (TupleValue args) =
  show $ parens $ hcat $ punctuate (text ",") $
  map (pretty allTypesL dID . varType) args

pretty :: Map ContractID SolidityTypesLayout -> DeclarationID ->
          SolidityBasicType -> Doc
pretty _ _ Boolean = text "bool"
pretty _ _ Address = text "address"
pretty _ _ (SignedInt s) = text "int" <> natural (s * 8)
pretty _ _ (UnsignedInt s) = text "uint" <> natural (s * 8)
pretty _ _ (FixedBytes s) = text "bytes" <> natural s
pretty _ _ DynamicBytes = text "bytes"
pretty _ _ String = text "string"
pretty allTypesL dID (FixedArray t l) = (pretty allTypesL dID t) <> text "[" <> natural l <> text "]"
pretty allTypesL dID (DynamicArray t) = (pretty allTypesL dID t) <> text "[]"
pretty allTypesL dID (Mapping d c) =
  text "mapping" <+> (parens $ pretty allTypesL dID d <+> text "=>" <+> pretty allTypesL dID c)
pretty allTypesL dID{declarationContract = cID} (Typedef name libM) = 
  case allTypesL Map.! (maybe dID (\l -> dID{declarationContract = cID{contractRealName = l} }) libM){declarationRealName = name} of
    EnumLayout s -> pretty typesL allTypesL (UnsignedInt s)
    _ -> text name

natural = integer . toInteger
