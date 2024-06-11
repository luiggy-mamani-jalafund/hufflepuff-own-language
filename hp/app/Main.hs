module Main (main) where

import Lib (funcParams, funcParam)
import Text.Parsec
import Lexer

main :: IO ()
main = do
  -- let input = "title:StringId,nose:StringId"
  input <- readFile "/home/fundacion/University/Fifth/ProgrammingLenguages/hufflepuff-own-language/hp/code"
  print input
  print $ parse funcParams "Error" input


  -- print input
  -- print $ parse (sepBy (many (letter <|> space)) (char ',')) "Error" "StringIdSpace ,description,state,Tag"
