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
    takeTaskAttribute
  )
import Text.Parsec

main :: IO ()
main = do
  -- let input = "title:StringId,nose:StringId"
  input <- readFile "/home/fundacion/University/Fifth/ProgrammingLenguages/hufflepuff-own-language/hp/code"
  -- print input
  print $ parse funcBody "Error" input

-- print input
-- print $ parse (sepBy (many (letter <|> space)) (char ',')) "Error" "StringIdSpace ,description,state,Tag"
