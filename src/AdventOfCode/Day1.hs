{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day1 where

import Internal.Prelude
import Data.Maybe
import Data.List (isPrefixOf, isSuffixOf)

data Direction = FromLeft | FromRight

findFirstInt :: Direction -> String -> Maybe Int
findFirstInt  _ "" = Nothing
findFirstInt dir line
  | "0" `operator` line = Just 0
  | "zero" `operator` line = Just 0
  | "1" `operator` line = Just 1
  | "one" `operator` line = Just 1
  | "2" `operator` line = Just 2
  | "two" `operator` line = Just 2
  | "3" `operator` line = Just 3
  | "three" `operator` line = Just 3
  | "4" `operator` line = Just 4
  | "four" `operator` line = Just 4
  | "5" `operator` line = Just 5
  | "five" `operator` line = Just 5
  | "6" `operator` line = Just 6
  | "six" `operator` line = Just 6
  | "7" `operator` line = Just 7
  | "seven" `operator` line = Just 7
  | "8" `operator` line = Just 8
  | "eight" `operator` line = Just 8
  | "9" `operator` line = Just 9
  | "nine" `operator` line = Just 9
    where operator = case dir of
            FromRight -> isSuffixOf
            FromLeft -> isPrefixOf
findFirstInt FromLeft line = findFirstInt FromLeft $ tail line
findFirstInt FromRight line = findFirstInt FromRight $ take (length line - 1) line

parseLine :: String -> Int
parseLine line = read (show (fromJust $ findFirstInt FromLeft line) <> show (fromJust $ findFirstInt FromRight line))

exampleData :: [String]
exampleData = [
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
    ]

main :: [String] -> String
main inputData = do
    let out = sum $ parseLine <$> inputData
    show out
