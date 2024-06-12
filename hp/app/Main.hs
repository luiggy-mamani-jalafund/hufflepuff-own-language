module Main (main) where

import Lexer
import Lib
  ( funcBody,
    funcParam,
    funcParams,
    member,
    task',
    taskMembers,
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
