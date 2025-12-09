module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6

import Flow
import Test.Hspec

main :: IO ()
main = hspec $ do
  day1Spec
  day2Spec
  day3Spec
  day4Spec
  day5Spec
  day6Spec

day1Spec :: Spec
day1Spec = do
  describe "day 1 tests" $ do
    it "returns 0 if it never stops on 0" $ do
      ["L10", "R20"]
        |> Day1.execute
        |> fst
        |> (`shouldBe` 0)

    it "returns 1 if it stops on 0 once" $ do
      ["L50"]
        |> Day1.execute
        |> fst
        |> (`shouldBe` 1)

    it "returns 2 if it stops on 0 twice" $ do
      ["L50", "R50", "L50"]
        |> Day1.execute
        |> fst
        |> (`shouldBe` 2)

    it "solves the test input" $ do
      ["L68", "L30", "R48", "L5 ", "R60", "L55", "L1 ", "L99", "R14", "L82"]
        |> Day1.execute
        |> fst
        |> (`shouldBe` 3)

    it "returns 1 if it goes through 0 once" $ do
      ["R60"]
        |> Day1.execute
        |> snd
        |> (`shouldBe` 1)

    it "solves the test input for part 2" $ do
      ["L68", "L30", "R48", "L5 ", "R60", "L55", "L1 ", "L99", "R14", "L82"]
        |> Day1.execute
        |> snd
        |> (`shouldBe` 6)

    it "returns 2 if it goes through 0 twice in 1 turn" $ do
      ["R150"]
        |> Day1.execute
        |> snd
        |> (`shouldBe` 2)

day2Spec :: Spec
day2Spec = do
    describe "day 2 tests" $ do
        it "returns 1 for 1 invalid id" $ do
            ["11-13"]
              |> Day2.execute
              |> fst
              |> (`shouldBe` 11)

        it "returns 2 for 2 invalid ids" $ do
            ["11-22"]
              |> Day2.execute
              |> fst
              |> (`shouldBe` 11+22)

        it "returns invalid id number in range" $ do
            ["10-33"]
              |> Day2.execute
              |> fst
              |> (`shouldBe` 11+22+33)

        it "sums multiple ranges" $ do
            ["11-13,22-33"]
              |> Day2.execute
              |> fst
              |> (`shouldBe` 11+22+33)

        it "solves the test input" $ do
            ["11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"]
              |> Day2.execute
              |> fst
              |> (`shouldBe` 1227775554)

        it "solves the test input for part 2" $ do
            ["11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"]
              |> Day2.execute
              |> snd
              |> (`shouldBe` 4174379265)

day3Spec :: Spec
day3Spec = do
    describe "day 3 tests" $ do
        it "returns 98 for 987654321111111" $ do
            ["987654321111111"]
                |> Day3.execute
                |> fst
                |> (`shouldBe` 98)

        it "returns 89 for 811111111111119" $ do
            ["811111111111119"]
                |> Day3.execute
                |> fst
                |> (`shouldBe` 89)

        it "returns 78 for 234234234234278" $ do
            ["818181911112111"]
                |> Day3.execute
                |> fst
                |> (`shouldBe` 92)

        it "solves the test input" $ do
            ["987654321111111",
             "811111111111119",
             "234234234234278",
             "818181911112111"]
                |> Day3.execute
                |> fst
                |> (`shouldBe` 357)

        it "returns 987654321111 for 987654321111111" $ do
            ["987654321111111"]
                |> Day3.execute
                |> snd
                |> (`shouldBe` 987654321111)

day4Spec :: Spec
day4Spec = do
    describe "day 4 tests" $ do
        it "takes the roll if there is space around it" $ do
            ["...",
             ".@.",
             "..."]
                |> Day4.execute
                |> fst
                |> (`shouldBe` 1)

        it "does not take the roll if there is no space around it" $ do
            ["@@@",
             "@@@",
             "@@@"]
                |> Day4.execute
                |> fst
                |> (`shouldBe` 4)

        it "solves the test input" $ do
            ["..@@.@@@@.",
             "@@@.@.@.@@",
             "@@@@@.@.@@",
             "@.@@@@..@.",
             "@@.@@@@.@@",
             ".@@@@@@@.@",
             ".@.@.@.@@@",
             "@.@@@.@@@@",
             ".@@@@@@@@.",
             "@.@.@@@.@."]
                |> Day4.execute
                |> fst
                |> (`shouldBe` 13)

        it "removes the already counted rolls" $ do
            ["@.@",
             "@@.",
             "@.@"]
                |> Day4.execute
                |> snd
                |> (`shouldBe` 6)

day5Spec :: Spec
day5Spec = do
    describe "day 5 tests" $ do
        it "returns 1 for 1 fresh ingredient" $ do
            ["3-5", "", "4"]
              |> Day5.execute
              |> fst
              |> (`shouldBe` 1)

        it "returns 0 for 0 fresh ingredient" $ do
            ["3-5", "", "7"]
              |> Day5.execute
              |> fst
              |> (`shouldBe` 0)

        it "solves the test input" $ do
            ["1-3",
             "10-14",
             "16-20",
             "12-18",
             "",
             "1",
             "5",
             "8",
             "11",
             "17",
             "32" ]
              |> Day5.execute
              |> fst
              |> (`shouldBe` 3)

        it "solves the test input for part 2" $ do
            ["1-3",
             "10-14",
             "16-20",
             "12-18",
             "",
             "1",
             "5",
             "8",
             "11",
             "17",
             "32" ]
                |> Day5.execute
                |> snd
                |> (`shouldBe` 14)

day6Spec :: Spec
day6Spec = do
    describe "day 6 tests" $ do
        it "multiplies a column" $ do
            ["123",
             " 45",
             "  6",
             "*  "]
                |> Day6.execute
                |> fst
                |> (`shouldBe` 123 * 45 * 6)

        it "solves the test input" $ do
            ["123 328  51 64 ",
             " 45 64  387 23 ",
             "  6 98  215 314",
             "*   +   *   +  "]
                |> Day6.execute
                |> fst
                |> (`shouldBe` 4277556)

        it "sums the column for part 2" $ do
            ["64 ",
             "23 ",
             "314",
             "+  "]
                |> Day6.execute
                |> snd
                |> (`shouldBe` 4 + 431 + 623)

        it "solves the test input for part 2" $ do
            ["123 328  51 64 ",
             " 45 64  387 23 ",
             "  6 98  215 314",
             "*   +   *   +  "]
                |> Day6.execute
                |> snd
                |> (`shouldBe` 3263827)
