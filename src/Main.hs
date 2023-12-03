module Main where

import AdventOfCode.Day1 qualified as Day1
import Internal.Prelude

main :: IO ()
main = do
    let day :: Int
        day = 1

    inputData <- readFile $ "./Data/Day" <> show day <> ".txt"

    let res = case day of
            1 -> Day1.main $ lines inputData
            _ -> undefined

    putStrLn $ "Day " <> show day <> " Result: " <> res
