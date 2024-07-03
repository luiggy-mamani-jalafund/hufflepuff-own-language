module Main (main) where

import HpParser (parseCode)
import SemanticAnalyzer (verifyProgram)
import SymbolTable (SymbolTable, emptyTable, printSymbolTable)
import Text.Parsec (parse)

main :: IO ()
main = do
    path <- readFile "code.hp"
    case parse parseCode "Error" path of
        Left err -> print err
        Right (parsedCode, initialSymbolTable) -> do
            case verifyProgram parsedCode of
                Left semanticErrors -> do
                    mapM_ print semanticErrors
                    putStrLn "Semantic errors found:"
                Right finalSymbolTable -> do
                    print parsedCode
                    mapM_ putStrLn (printSymbolTable finalSymbolTable)