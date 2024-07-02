{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec
import MemberSpec (spec)

main :: IO ()
main = hspec spec