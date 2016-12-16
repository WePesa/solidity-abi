-- |
-- Module: Selector
-- Description: Source for the calculator for the 4-byte function hash
-- Maintainer: Ryan Reich <ryan@blockapps.net
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Selector (selector) where

import Control.Monad.Trans.Reader

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding
import Text.PrettyPrint

import qualified Crypto.Hash.SHA3 as SHA3 (hash)
import Numeric

import SolidityTypes
-- | The 'selector' function is responsible for producing the 4-byte
-- hash that Solidity uses to identify functions.  It's essentially the
-- first 4 bytes of the Keccak hash of the signature with all the argument
-- names, modifiers, and storage specifiers removed.  If the signature
-- contains 'enum' or contract type names, however, they are converted to,
-- respectively 'uintX' and 'address' types, where X is the least number of
-- bytes (in bits) that can hold all the enum's values.  Structs are not
-- permitted at all.
selector :: LinkageT 'AfterLayout -> Identifier -> Tuple -> String
selector linkage name args = hash4 $ runReader (signature name args) linkage
  where
    hash4 bs = concatMap toHex $ BS.unpack $ BS.take 4 $ SHA3.hash 256 bs
    toHex = zeroPad . flip showHex ""
    zeroPad [c] = ['0',c]
    zeroPad x = x

type SelectorReader = Reader (LinkageT 'AfterLayout)

signature :: Identifier -> Tuple -> SelectorReader ByteString
signature name args = do
  argsSig <- sigArgTypes args
  return $ encodeUtf8 $ T.pack $ name ++ argsSig

sigArgTypes :: Tuple -> SelectorReader String
sigArgTypes (TupleValue args) =
  show . parens . hcat . punctuate (text ",") <$>
  mapM (sig . argType) args

sig :: BasicType -> SelectorReader Doc
sig Boolean = return $ text "bool"
sig Address = return $ text "address"
sig (SignedInt s) = return $ text "int" <> natural (s * 8)
sig (UnsignedInt s) = return $ text "uint" <> natural (s * 8)
sig (FixedBytes s) = return $ text "bytes" <> natural s
sig DynamicBytes = return $ text "bytes"
sig String = return $ text "string"
sig (FixedArray t l) = do
  tSig <- sig t
  return $ tSig <> text "[" <> natural l <> text "]"
sig (DynamicArray t) = do
  tSig <- sig t
  return $ tSig <> text "[]"
sig (Mapping d c) = do
  dSig <- sig d
  cSig <- sig c
  return $ text "mapping" <+> parens (dSig <+> text "=>" <+> cSig)
sig (LinkT linkID) = do
  linkage <- ask
  case linkage Map.! linkID of
    ContractLink _ -> sig Address
    InheritedLink WithSize{sizeOf} -> sig $ UnsignedInt sizeOf
    PlainLink WithSize{sizeOf} -> sig $ UnsignedInt sizeOf
    LibraryLink WithSize{sizeOf} -> sig $ UnsignedInt sizeOf
    _ -> error "Found what I thought should be an enum but without its size"

natural = integer . toInteger
