module Main where

import Day1
import Day2
import Test.Hspec

main :: IO ()
main = hspec $ do
  day1Spec
  day2Spec

day1Spec :: Spec
day1Spec = do
  describe "day 1 tests" $ do
    it "returns 0 if it never stops on 0" $ do
      fst (Day1.execute ["L10", "R20"]) `shouldBe` 0

    it "returns 1 if it stops on 0 once" $ do
      fst (Day1.execute ["L50"]) `shouldBe` 1

    it "returns 2 if it stops on 0 twice" $ do
      fst (Day1.execute ["L50", "R50", "L50"]) `shouldBe` 2

    it "solves the test input" $ do
      fst (Day1.execute ["L68", "L30", "R48", "L5 ", "R60", "L55", "L1 ", "L99", "R14", "L82"]) `shouldBe` 3

    it "returns 1 if it goes through 0 once" $ do
      snd (Day1.execute ["R60"]) `shouldBe` 1

    it "solves the test input for part 2" $ do
      snd (Day1.execute ["L68", "L30", "R48", "L5 ", "R60", "L55", "L1 ", "L99", "R14", "L82"]) `shouldBe` 6

    it "returns 2 if it goes through 0 twice in 1 turn" $ do
      snd (Day1.execute ["R150"]) `shouldBe` 2

day2Spec :: Spec
day2Spec = do
    describe "day 2 tests" $ do
        it "returns 1 for 1 invalid id" $ do
            fst (Day2.execute "11-13") `shouldBe` 1

        it "returns 2 for 2 invalid ids" $ do
            fst (Day2.execute "11-22") `shouldBe` 2
