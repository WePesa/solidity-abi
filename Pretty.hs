{-# LANGUAGE FlexibleInstances #-}

module Pretty (
  Pretty(pretty),
  canonicalSignature
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Text.PrettyPrint

import ParserTypes

class Pretty a where
  pretty :: a -> Doc

instance Pretty SolidityContract where
  pretty (Contract name abi) =
    text "contract" <+> text name $+$ nest 2 (vcat $ map pretty abi)

instance Pretty SoliditySymbol where
  pretty (Variable name vType) =
    pretty vType <+> text name
  pretty (Function name args returns) =
    text name <+> 
    maybe empty (\r -> text "returns" <+> parens (pretty r)) returns <+>
    (parens $ hsep $ punctuate (text ",") $ map pretty args)

instance Pretty SolidityType where
  pretty Boolean = text "bool"
  pretty Address = text "address"
  pretty (SignedInt s) = text "int" <> integer (s * 8)
  pretty (UnsignedInt s) = text "uint" <> integer (s * 8)
  pretty (FixedBytes s) = text "bytes" <> integer s
  pretty DynamicBytes = text "bytes"
  pretty String = text "string"
  pretty (SignedReal s p) = text "real" <> integer ((s - p) * 8) <> text "x" <> integer (p * 8)
  pretty (UnsignedReal s p) = text "ureal" <> integer ((s - p) * 8) <> text "x" <> integer (p * 8)
  pretty (FixedArray t l) = (pretty t) <> text "[" <> integer l <> text "]"
  pretty (DynamicArray t) = (pretty t) <> text "[]"
  pretty (Mapping d c) =
    text "mapping" <+>
    (parens $ pretty d <+> text "=>" <+> pretty c)
  pretty (Enum names) =
    text "enum" <+>
    (braces $ hsep $ punctuate (text ",") $
     [ text name <+> text "=" <+> integer n | (name, n) <- zip names [0..] ])
  pretty (Struct fields) =
    text "struct" <+> (braces $ hsep $ punctuate (text ";") $ map pretty fields)
  pretty (UserDefined name) = text name

instance Pretty [SolidityContract] where
  pretty = vcat . intersperse (text "") . map pretty

canonicalSignature :: SoliditySymbol -> ByteString
canonicalSignature (Function name args _)
  = encodeUtf8 $ T.pack $ name ++ prettyArgTypes
  where prettyArgTypes = show $ parens $ hcat $ punctuate (text ",") $
                         map (pretty . varType) args
