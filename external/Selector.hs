{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Selector (selector) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Text.PrettyPrint

import qualified Crypto.Hash.SHA3 as SHA3 (hash)
import Numeric

import ParserTypes
import LayoutTypes

import Debug.Trace

selector :: SolidityTypesLayout -> Identifier -> [SolidityObjDef] -> [SolidityObjDef] -> String
selector typesL name args vals = hash4 $ signature typesL name args vals
  where
    hash4 bs = concatMap toHex $ BS.unpack $ BS.take 4 $ SHA3.hash 256 bs
    toHex = zeroPad . flip showHex ""
    zeroPad [c] = ['0',c]
    zeroPad x = x

signature :: SolidityTypesLayout -> Identifier -> [SolidityObjDef] -> [SolidityObjDef] -> ByteString
signature typesL name args _ = encodeUtf8 $ T.pack $ name ++ prettyArgTypes typesL args

prettyArgTypes :: SolidityTypesLayout -> [SolidityObjDef] -> String
prettyArgTypes typesL args =
  show $ parens $ hcat $ punctuate (text ",") $
  catMaybes $ map (fmap (pretty typesL) . varType) args

varType :: SolidityObjDef -> Maybe SolidityBasicType
varType (ObjDef _ (SingleValue t) NoValue _) = Just t
varType _ = Nothing

pretty :: SolidityTypesLayout -> SolidityBasicType -> Doc
pretty _ Boolean = text "bool"
pretty _ Address = text "address"
pretty _ (SignedInt s) = text "int" <> natural (s * 8)
pretty _ (UnsignedInt s) = text "uint" <> natural (s * 8)
pretty _ (FixedBytes s) = text "bytes" <> natural s
pretty _ DynamicBytes = text "bytes"
pretty _ String = text "string"
pretty typesL (FixedArray t l) = (pretty typesL t) <> text "[" <> natural l <> text "]"
pretty typesL (DynamicArray t) = (pretty typesL t) <> text "[]"
pretty typesL (Mapping d c) =
  text "mapping" <+> (parens $ pretty typesL d <+> text "=>" <+> pretty typesL c)
pretty typesL (Typedef name) = 
  case typesL Map.! name of
    ContractTLayout{} -> pretty typesL Address
    EnumLayout s -> pretty typesL (UnsignedInt s)
    _ -> text name

natural = integer . toInteger
