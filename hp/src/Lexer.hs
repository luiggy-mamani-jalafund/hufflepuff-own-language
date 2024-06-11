module Lexer where

import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

languageDef :: P.GenLanguageDef String () Identity
languageDef =
  emptyDef
    { P.commentLine = "//",
      P.nestedComments = True,
      P.identStart = letter,
      P.identLetter = alphaNum,
      P.opStart = oneOf ":,.{}()[]-=!<>",
      P.opLetter = oneOf ":,.{}()[]-=!<>",
      P.reservedNames =
        [ "func",
          "params",
          "return",
          "if",
          "then",
          "else",
          "pattern",
          "case",
          "default",
          "do",
          "map"
        ],
      P.reservedOpNames =
        [ ":",
          ",",
          ".",
          "{",
          "}",
          "(",
          ")",
          "[",
          "]",
          "->",
          "==",
          "!=",
          "<",
          ">",
          "<=",
          ">="
        ],
      P.caseSensitive = True
    }

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser languageDef

identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

integer :: Parser Integer
integer = P.integer lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

dataType :: Parser String
dataType = choice (map (try . string) sortedDataTypes)
  where
    sortedDataTypes =
      [ "StringIdSpace",
        "StringParagraph",
        "StringId",
        "State",
        "Bool",
        "Member",
        "Tag",
        "Task",
        "List:Task",
        "List:List",
        "List:StringIdSpace",
        "List:StringParagraph",
        "List:StringId",
        "List:State",
        "List:Bool",
        "List:Member",
        "List:Tag"
      ]