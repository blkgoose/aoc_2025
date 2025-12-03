module Main where

import Test.Hspec
import Day1

main :: IO ()
main = hspec $ do
    describe "gives the password" $ do
        it "returns 0 if it never stops on 0" $ do
            (fst $ execute ["L10", "R20"]) `shouldBe` 0

        it "returns 1 if it stops on 0 once" $ do
            (fst $ execute ["L50"]) `shouldBe` 1

        it "returns 2 if it stops on 0 twice" $ do
            (fst $ execute ["L50", "R50", "L50"]) `shouldBe` 2

        it "solves the test input" $ do
            (fst $ execute [ "L68", "L30", "R48", "L5 ", "R60", "L55", "L1 ", "L99", "R14", "L82" ]) `shouldBe` 3

        it "returns 1 if it goes through 0 once" $ do
            (snd $ execute ["R60"]) `shouldBe` 1

        it "solves the test input for part 2" $ do
            (snd $ execute [ "L68", "L30", "R48", "L5 ", "R60", "L55", "L1 ", "L99", "R14", "L82" ]) `shouldBe` 6
