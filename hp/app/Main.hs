module Main (main) where

import CodeGenerator
import HpParser (parseCode)
import System.Process (readProcess)
import Text.Parsec (parse)

executeJavaScript :: FilePath -> IO String
executeJavaScript filePath = readProcess "node" [filePath] ""

main :: IO ()
main = do
  path <- readFile "code.hp"
  case parse parseCode "Error" path of
    Left err -> print err
    Right (parsedCode, finalSymbolTable) -> do
      let codeGenerated = generateCode parsedCode
      let generatedPath = "build/generated.js"
      writeFile generatedPath codeGenerated
      result <- executeJavaScript generatedPath
      putStrLn result

-- print parsedCode
-- print finalSymbolTable
-- print codeGenerated

