module Main where

import AdventOfCode.Day1 qualified as Day1
import AdventOfCode.Day2 qualified as Day2
import AdventOfCode.Day3 qualified as Day3
import AdventOfCode.Day4 qualified as Day4
import AdventOfCode.Day5 qualified as Day5

import Internal.Prelude

main :: IO ()
main = do
    let day :: Int
        day = 5

    inputData <- readFile $ "./Data/Day" <> show day <> ".txt"

    let res = case day of
            1 -> Day1.main $ lines inputData
            2 -> Day2.main $ lines inputData
            3 -> Day3.main $ lines inputData
            4 -> Day4.main $ lines inputData
            5 -> Day5.main $ lines inputData
            _ -> undefined

    putStrLn $ "Day " <> show day <> " Result: " <> res
