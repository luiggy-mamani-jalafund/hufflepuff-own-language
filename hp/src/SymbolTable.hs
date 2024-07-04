{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SymbolTable where

import AbstractSyntaxTree
    ( BoolExpression,
      FuncBody,
      FuncParam,
      List,
      Member,
      Task,
      Value,
      Type,
      Identifier,
      Literal,
      Statement )
import qualified Data.Map as M

data SymbolInfo
    = VariableInfo Identifier Type (Maybe Value)
    | FunctionInfo Identifier Type [FuncParam] FuncBody
    | TaskInfo Identifier Task
    | MemberInfo Identifier Member
    | ListInfo Identifier List
    | BoolExpressionInfo BoolExpression
    | LiteralInfo Literal
    | DoAssignmentInfo Identifier Type Statement
    | FuncParamInfo Identifier Type
    deriving (Show)

newtype SymbolTable = SymbolTable [M.Map Identifier SymbolInfo]
    deriving (Show)

emptyTable :: SymbolTable
emptyTable = SymbolTable [M.empty]

insertVariable :: Identifier -> Type -> Maybe Value -> SymbolTable -> SymbolTable
insertVariable name typ val (SymbolTable (current:rest)) =
    SymbolTable (M.insert name (VariableInfo name typ val) current:rest)
insertVariable _ _ _ st = st

insertFunction :: Identifier -> Type -> [FuncParam] -> FuncBody -> SymbolTable -> SymbolTable
insertFunction name retType params body (SymbolTable (current:rest)) =
    SymbolTable (M.insert name (FunctionInfo name retType params body) current:rest)
insertFunction _ _ _ _ st = st

insertTask :: Identifier -> Task -> SymbolTable -> SymbolTable
insertTask name task (SymbolTable (current:rest)) =
    SymbolTable (M.insert name (TaskInfo name task) current:rest)
insertTask _ _ st = st

insertMember :: Identifier -> Member -> SymbolTable -> SymbolTable
insertMember name member (SymbolTable (current:rest)) =
    SymbolTable (M.insert name (MemberInfo name member) current:rest)
insertMember _ _ st = st

insertList :: Identifier -> List -> SymbolTable -> SymbolTable
insertList name list (SymbolTable (current:rest)) =
    SymbolTable (M.insert name (ListInfo name list) current:rest)
insertList _ _ st = st

insertBoolExpression :: Identifier -> BoolExpression -> SymbolTable -> SymbolTable
insertBoolExpression name boolExpr (SymbolTable (current:rest)) =
    SymbolTable (M.insert name (BoolExpressionInfo boolExpr) current:rest)
insertBoolExpression _ _ st = st

insertLiteral :: Identifier -> Literal -> SymbolTable -> SymbolTable
insertLiteral name lit (SymbolTable (current:rest)) =
    SymbolTable (M.insert name (LiteralInfo lit) current:rest)
insertLiteral _ _ st = st

insertDoAssignment :: Identifier -> Type -> Statement -> SymbolTable -> SymbolTable
insertDoAssignment name typ stmt (SymbolTable (current:rest)) =
    SymbolTable (M.insert name (DoAssignmentInfo name typ stmt) current:rest)
insertDoAssignment _ _ _ st = st

insertFuncParam :: Identifier -> Type -> SymbolTable -> SymbolTable
insertFuncParam id typ (SymbolTable (current:rest)) =
    SymbolTable (M.insert id (FuncParamInfo id typ) current:rest)
insertFuncParam _ _ st = st

lookupSymbol :: Identifier -> SymbolTable -> Maybe SymbolInfo
lookupSymbol name (SymbolTable scopes) = lookupInScopes name scopes
  where
    lookupInScopes _ [] = Nothing
    lookupInScopes name (scope:rest) =
        case M.lookup name scope of
            Just info -> Just info
            Nothing -> lookupInScopes name rest

removeSymbol :: Identifier -> SymbolTable -> SymbolTable
removeSymbol name (SymbolTable (current:rest)) = SymbolTable (M.delete name current : rest)
removeSymbol _ st = st


enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable scopes) = SymbolTable (M.empty : scopes)

exitScope :: SymbolTable -> SymbolTable
exitScope (SymbolTable (_:rest)) = SymbolTable rest
exitScope st = st

lookupCurrentScope :: Identifier -> SymbolTable -> Maybe SymbolInfo
lookupCurrentScope name (SymbolTable (current:_)) = M.lookup name current
lookupCurrentScope _ _ = Nothing


printSymbolTable :: SymbolTable -> [String]
printSymbolTable (SymbolTable scopes) = concatMap printScope scopes
  where
    printScope scope = M.foldrWithKey (\key val acc -> (show key ++ " : " ++ show val) : acc) [] scope
