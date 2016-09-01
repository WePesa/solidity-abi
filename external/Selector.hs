{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Selector (selector) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Control.Monad.Trans.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Text.PrettyPrint

import qualified Crypto.Hash.SHA3 as SHA3 (hash)
import Numeric

import SolidityTypes

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
  return $ text "mapping" <+> (parens $ dSig <+> text "=>" <+> cSig)
sig (LinkT linkID) = do
  linkage <- ask
  case linkage Map.! linkID of
    ContractLink _ -> sig Address
    InheritedLink WithSize{sizeOf} -> sig $ UnsignedInt sizeOf
    PlainLink WithSize{sizeOf} -> sig $ UnsignedInt sizeOf
    LibraryLink WithSize{sizeOf} -> sig $ UnsignedInt sizeOf

natural = integer . toInteger
