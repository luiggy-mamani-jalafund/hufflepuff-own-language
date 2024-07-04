module ErrorHandler where

import AbstractSyntaxTree (Identifier)

data SemanticError
    = UndeclaredVariable Identifier
    | UndeclaredFunction Identifier
    | AlreadyDeclared Identifier
    deriving (Show)

type SemanticResult a = Either [SemanticError] a
