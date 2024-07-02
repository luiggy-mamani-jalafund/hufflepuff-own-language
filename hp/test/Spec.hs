{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec
import TaskSpec (spec)

main :: IO ()
main = hspec spec
