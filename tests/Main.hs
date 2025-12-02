module Main where

import Test.Hspec
import Day1

main :: IO ()
main = hspec $ do
    describe "gives the password" $ do
        it "returns 1 if it stops on 0 once" $ do
            execute ["L50"] `shouldBe` 1
