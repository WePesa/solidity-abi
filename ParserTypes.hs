{-# LANGUAGE FlexibleInstances #-}

module ParserTypes where

import Text.PrettyPrint
import Data.List

data SolidityContract =
  Contract { contractName :: String, contractABI :: [SoliditySymbol] }
  deriving (Show)

data SoliditySymbol =
  Variable { varName :: String, varType :: SolidityType } |
  Function { funcName :: String,
             args :: [SoliditySymbol], returns :: Maybe SolidityType }
  deriving (Show)

data SolidityType =
  Boolean |
  Address |
  SignedInt   { size :: Integer } |
  UnsignedInt { size :: Integer } |
  Bytes       { size :: Integer } |
  FixedArray  { elemType :: SolidityType } |
  DynamicArray{ elemType :: SolidityType } |
  Mapping     { domType  :: SolidityType, codType :: SolidityType } |
  Enum        { names  :: [String] } |
  Struct      { fields :: [SoliditySymbol] }
  deriving (Show)

data SolidityTypeExtended =
  DeclaredType { declType :: SolidityType } |
  FunctionType { funcArgs :: [SolidityType], funcRet :: Maybe SolidityType } |
  ContractType { implemented :: [SoliditySymbol], abstract :: [SoliditySymbol] }

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
  pretty (SignedInt s) = text "int" <> integer s
  pretty (UnsignedInt s) = text "uint" <> integer s
  pretty (Bytes s) = text "bytes" <> integer s
  pretty (FixedArray t) = (pretty t) <> text "[#]"
  pretty (DynamicArray t) = (pretty t) <> text "[]"
  pretty (Mapping d c) = parens $ pretty d <+> text "=>" <+> pretty c
  pretty (Enum names) = braces $ hsep $ punctuate (text ",") $
                        [ text name <+> text "=" <+> integer n |
                          (name, n) <- zip names [0..] ]
  pretty (Struct fields) = braces $ hsep $ punctuate (text ";") $ map pretty fields

instance Pretty [SolidityContract] where
  pretty = vcat . intersperse (text "") . map pretty
