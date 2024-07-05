module Main (main) where

import CodeGenerator
import HpParser (parseCode)
import System.Process (readProcess)
import Text.Parsec (parse)

executeJavaScript :: FilePath -> IO String
executeJavaScript filePath = readProcess "node" [filePath] ""

executeProject :: String -> String -> IO ()
executeProject name path = do
  putStrLn name
  path <- readFile path
  case parse parseCode "Error" path of
    Left err -> print err
    Right (parsedCode, finalSymbolTable) -> do
      let codeGenerated = generateCode parsedCode
      let generatedPath = "build/generated.js"
      writeFile generatedPath codeGenerated
      result <- executeJavaScript generatedPath
      putStrLn result

executeExample :: IO()
executeExample = do executeProject "___Example___" "code.hp"

executeSprint1 :: IO ()
executeSprint1 = do executeProject "___Sprint 1___" "project/sprint_1.hp"

executeSprint2 :: IO ()
executeSprint2 = do executeProject "___Sprint 2___" "project/sprint_2.hp"

executeSprint3 :: IO ()
executeSprint3 = do executeProject "___Sprint 3___" "project/sprint_3.hp"

executeSprint4 :: IO ()
executeSprint4 = do executeProject "___Sprint 4___" "project/sprint_4.hp"

main :: IO ()
main = do
  executeExample
  executeSprint1
  executeSprint2
  executeSprint3
  executeSprint4
