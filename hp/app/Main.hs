module Main (main) where

import HpParser (parseCode)
import Text.Parsec ( parse )

main :: IO ()
main = do
  path <- readFile "code.hp"
  case parse parseCode "Error" path of
    Left err -> print err
    Right (parsedCode, finalSymbolTable) -> do
      print parsedCode
      print "----"
      print finalSymbolTable 