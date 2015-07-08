{-# LANGUAGE FlexibleInstances #-}

module Debug (
  Pretty(pretty)
  ) where

import Data.List 
import Text.PrettyPrint

import ParserTypes

class Pretty a where
  pretty :: a -> Doc

instance Pretty SolidityContract where
  pretty (Contract name abi) =
    text "contract" <+> text name $+$ nest 2 (vcat $ map pretty abi)

instance Pretty SoliditySymbol where
  pretty (Variable name vType) =
    text name <+> text "::" <+> pretty vType
  pretty (Function name args returns) =
    text name <+> text "::" <+>
    (parens $ hsep $ punctuate (text ",") $ map pretty args) <+>
    text "->" <+>
    maybe (text "()") pretty returns

instance Pretty SolidityType where
  pretty Boolean = text "boolean"
  pretty Address = text "address"
  pretty String = text "string"
  pretty (SignedInt s) = text "int" <> integer (s * 8)
  pretty (UnsignedInt s) = text "uint" <> integer (s * 8)
  pretty (FixedBytes s) = text "bytes" <> integer s
  pretty DynamicBytes = text "bytes"
  pretty (SignedReal s p) = text "real" <> integer ((s - p) * 8) <> text "x" <> integer (p * 8)
  pretty (UnsignedReal s p) = text "ureal" <> integer ((s - p) * 8) <> text "x" <> integer (p * 8)
  pretty (FixedArray t l) = (pretty t) <> text "[" <> integer l <> text "]"
  pretty (DynamicArray t) = (pretty t) <> text "[]"
  pretty (Mapping d c) = parens $ pretty d <+> text "=>" <+> pretty c
  pretty (Enum names) = braces $ hsep $ punctuate (text ",") $
                        [ text name <+> text "=" <+> integer n |
                          (name, n) <- zip names [0..] ]
  pretty (Struct fields) = braces $ hsep $ punctuate (text ";") $ map pretty fields

instance Pretty [SolidityContract] where
  pretty = vcat . intersperse (text "") . map pretty
