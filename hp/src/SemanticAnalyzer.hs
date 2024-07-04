module SemanticAnalyzer where

import AbstractSyntaxTree
import SymbolTable
import HpParser (statement)

data SemanticError
    = UndeclaredVariable Identifier
    | UndeclaredFunction Identifier
    deriving (Show)

type SemanticResult a = Either [SemanticError] a

verifyStatements :: [Statement] -> SymbolTable -> SemanticResult SymbolTable
verifyStatements stmts table = foldl verifyStatement (Right table) stmts

verifyStatement :: SemanticResult SymbolTable -> Statement -> SemanticResult SymbolTable
verifyStatement (Left errors) statement = Left errors
verifyStatement (Right table) statement =
    case statement of
        SFuncCall (FuncCall name _) ->
            case lookupSymbol name table of
                Nothing -> Left [UndeclaredFunction name]
                Just _  -> Right table
        SValue (ValLiteral (LStringIdentifier (StringId name))) ->
            case lookupSymbol name table of
                Nothing -> Left [UndeclaredVariable name]
                Just _  -> Right table
        SValue _ -> Right table
        STakeTaskAttribute (TakeTaskAttributeStrings (TakeTaskAttributeTitle name)) ->
            case lookupSymbol name table of
                Nothing -> Left [UndeclaredVariable name]
                Just _  -> Right table
        STakeMemberAttribute (TakeMemberAttributeName name) ->
            case lookupSymbol name table of
                Nothing -> Left [UndeclaredVariable name]
                Just _  -> Right table
        STakeMemberAttribute (TakeMemberAttributeRole name) ->
            case lookupSymbol name table of
                Nothing -> Left [UndeclaredVariable name]
                Just _  -> Right table
        SBoolCondition (Condition cond thenStmt elseStmt) -> do
            table' <- verifyStatement (Right table) (SBoolExp cond)
            table'' <- verifyStatement (Right table') thenStmt
            verifyStatement (Right table'') elseStmt
        SCycle (Cycle mapF (CycleId name)) ->
            case lookupSymbol name table of
                Nothing -> Left [UndeclaredVariable name]
                Just _  -> Right table
        _ -> Right table

verifyFunctions :: [Func] -> SymbolTable -> SemanticResult SymbolTable
verifyFunctions funcs table = foldl verifyFunction (Right table) funcs

verifyFunction :: SemanticResult SymbolTable -> Func -> SemanticResult SymbolTable
verifyFunction (Left errors) _ = Left errors
verifyFunction (Right table) (Func name retType params body) = do
    let table' = insertFunction name retType params body table
    let paramTable = foldl (\acc (FuncParam paramId paramType) -> insertFuncParam paramId paramType acc) (enterScope table') params
    verifyFuncBody body paramTable

verifyFuncBody :: FuncBody -> SymbolTable -> SemanticResult SymbolTable
verifyFuncBody (FuncReturn statement) table = verifyStatement (Right table) statement
verifyFuncBody (FuncPattern cases def) table = do
    table' <- foldl verifyPatternCase (Right table) cases
    verifyPatternDefault def table'

verifyPatternCase :: SemanticResult SymbolTable -> PatternCase -> SemanticResult SymbolTable
verifyPatternCase (Left errors) _ = Left errors
verifyPatternCase (Right table) (PatternCase values statement) =
    verifyStatement (Right table) statement

verifyPatternDefault :: PatternDefault -> SymbolTable -> SemanticResult SymbolTable
verifyPatternDefault (PatternDefault statement) table = verifyStatement (Right table) statement

verifyDoStatements :: [DoStatement] -> SymbolTable -> SemanticResult SymbolTable
verifyDoStatements doStmts table = foldl verifyDoStatement (Right table) doStmts

verifyDoStatement :: SemanticResult SymbolTable -> DoStatement -> SemanticResult SymbolTable
verifyDoStatement (Left errors) _ = Left errors
verifyDoStatement (Right table) (DoAssignment name typ statement) =
    let table' = insertDoAssignment name typ statement table
    in verifyStatement (Right table') statement
verifyDoStatement (Right table) (DoPrint (PrintRef name)) =
    case lookupSymbol name table of
        Nothing -> Left [UndeclaredVariable name]
        Just _  -> Right table
verifyDoStatement (Right table) (DoPrint (PrintStatement statement)) =
    verifyStatement (Right table) statement


verifyProgram :: Code -> SemanticResult SymbolTable
verifyProgram (Code funcs (DoNotation doStmts)) = do
    table <- verifyFunctions funcs emptyTable
    verifyDoStatements doStmts table