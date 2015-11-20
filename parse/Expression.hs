module Expression where

import Text.Parsec
import Text.Parsec.Expr

import Lexer
import ParserTypes

intExpr :: SolidityParser Integer
intExpr = buildExpressionParser intTable intTerm

intTerm =  parens intTerm <|> natural

intTable = [ [prefix "-" negate, prefix "+" id ],
             [binary "**" (^) AssocRight],
             [binary "*" (*) AssocLeft,
              binary "/" (div) AssocLeft,
              binary "%" (mod) AssocLeft],
             [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft ]]
         
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })
