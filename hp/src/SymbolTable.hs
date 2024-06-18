{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SymbolTable where

import AbstractSyntaxTree
    ( BoolExpression,
      FuncBody,
      FunParam,
      List,
      Member,
      Task,
      Value,
      Type,
      Identifier,
      Literal )
import qualified Data.Map as M

data SymbolInfo
    = VariableInfo Identifier Type (Maybe Value)
    | FunctionInfo Identifier Type [FunParam] FuncBody
    | TaskInfo Identifier Task
    | MemberInfo Identifier Member
    | ListInfo Identifier List
    | BoolExpressionInfo BoolExpression
    | LiteralInfo Literal
    deriving (Show)

newtype SymbolTable = SymbolTable (M.Map Identifier SymbolInfo)
    deriving (Show)

emptyTable :: SymbolTable
emptyTable = SymbolTable M.empty

insertVariable :: Identifier -> Type -> Maybe Value -> SymbolTable -> SymbolTable
insertVariable name typ val (SymbolTable table) = 
    SymbolTable (M.insert name (VariableInfo name typ val) table)

insertFunction :: Identifier -> Type -> [FunParam] -> FuncBody -> SymbolTable -> SymbolTable
insertFunction name retType params body (SymbolTable table) =
    SymbolTable (M.insert name (FunctionInfo name retType params body) table)

insertTask :: Identifier -> Task -> SymbolTable -> SymbolTable
insertTask name task (SymbolTable table) =
    SymbolTable (M.insert name (TaskInfo name task) table)

insertMember :: Identifier -> Member -> SymbolTable -> SymbolTable
insertMember name member (SymbolTable table) =
    SymbolTable (M.insert name (MemberInfo name member) table)

insertList :: Identifier -> List -> SymbolTable -> SymbolTable
insertList name list (SymbolTable table) =
    SymbolTable (M.insert name (ListInfo name list) table)

insertBoolExpression :: BoolExpression -> SymbolTable -> SymbolTable
insertBoolExpression boolExpr (SymbolTable table) =
    SymbolTable (M.insert (show boolExpr) (BoolExpressionInfo boolExpr) table)

insertLiteral :: Identifier -> Literal -> SymbolTable -> SymbolTable
insertLiteral name lit (SymbolTable table) =
    SymbolTable (M.insert name (LiteralInfo lit) table)

lookupSymbol :: Identifier -> SymbolTable -> Maybe SymbolInfo
lookupSymbol name (SymbolTable table) = M.lookup name table

removeSymbol :: Identifier -> SymbolTable -> SymbolTable
removeSymbol name (SymbolTable table) = SymbolTable (M.delete name table)