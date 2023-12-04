module Main where

import AdventOfCode.Day1 qualified as Day1
import AdventOfCode.Day2 qualified as Day2
import Internal.Prelude

main :: IO ()
main = do
    let day :: Int
        day = 2

    inputData <- readFile $ "./Data/Day" <> show day <> ".txt"

    let res = case day of
            1 -> Day1.main $ lines inputData
            2 -> Day2.main $ lines inputData
            _ -> undefined

    putStrLn $ "Day " <> show day <> " Result: " <> res
