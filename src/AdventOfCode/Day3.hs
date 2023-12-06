{-# LANGUAGE OverloadedStrings  #-}
module AdventOfCode.Day3 where

import Internal.Prelude
import Data.Map.Strict hiding (mapMaybe)
import Data.Char

data Number = Number {
    num :: String,
    surroundings :: [Char]
}
    deriving stock (Show)

-- | X * Y
type Pos = (Int, Int)

type CharGrid = Map Pos Char

exampleData :: [String]
exampleData = [
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
    ]

parseLineToMap :: (Int, String) -> CharGrid
parseLineToMap (y, line) = fromList $ mapFst (,y) $ zip [0..] line

parseInputToMap :: [String] -> CharGrid
parseInputToMap input = unions $ parseLineToMap <$> zip [0..] input

consumeDigit :: [String] -> Either (Char, [String]) [String]
consumeDigit ((d:restOfString):restOfInput) | isNumber d = Left (d, restOfString:restOfInput)
consumeDigit input = Right input

surroundings :: CharGrid -> Pos -> [Char]
surroundings charGrid (posX, posY) =
    mapMaybe (charGrid !?) [(x, y) | x <- [posX - 1 .. posX + 1], y <- [posY - 1 .. posY + 1], x < y && not (x == posX && y == posY)]



parse :: CharGrid -> [Number] -> [Pos] -> [Number]
parse _ [] [] = []
parse _ (h:acc) []
    | h.num == "" = acc
    | otherwise = h:acc
parse charGrid [] (currentPos:positions) = do
    let nextChar = charGrid ! currentPos
    if isNumber nextChar
        then do
            let sur = surroundings charGrid currentPos
            let newCurrentNum = Number {
                num = [nextChar],
                surroundings = sur }
            parse charGrid [newCurrentNum] positions
        else do
            let newHead = Number { num = "", surroundings = []}
            parse charGrid [newHead] positions
parse charGrid (currentNum:acc) (currentPos:positions) = do
    let nextChar = charGrid ! currentPos
    if isNumber nextChar
        then do
            let sur = surroundings charGrid currentPos
            let newCurrentNum = Number {
                num = currentNum.num <> [nextChar],
                surroundings = currentNum.surroundings <> sur }
            parse charGrid (newCurrentNum : acc) positions
        else do
            let newHead = Number { num = "", surroundings = []}
            parse charGrid (newHead : currentNum : acc) positions

main :: [String] -> String
main inputs = do
    let charGrid = parseInputToMap inputs

    let dimensionsX = length inputs
    let dimensionsY = length (head inputs)
    let positions = [(x, y) | x <- [0..dimensionsX], y <- [0..dimensionsY], x < y]
    show $ parse charGrid [] positions

