module Main (main) where

import Lexer
import Lib
  ( 
    func,
    funcBody,
    funcParam,
    funcParams,
    member,
    task',
    taskMembers,
    statement,
    boolComparison,
    literal,
    takeTaskAttribute,
    casePattern,
    casePatternVal,
    casePatternVals,
    parseCode,
  )
import Text.Parsec

main :: IO ()
main = do
  input <- readFile "./code"
  case parse parseCode "Error" input of
    Left err -> print err
    Right (parsedCode, finalSymbolTable) -> do
      print parsedCode
      print finalSymbolTable 

