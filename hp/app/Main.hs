module Main (main) where

import HpParser (code)
import Text.Parsec ( parse )

main :: IO ()
main = do
  let path = "code.hp"
  input <- readFile path
  print $ parse code "Error" input