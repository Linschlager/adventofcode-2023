{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day3 where

import Data.Char
import Data.Map.Strict hiding (filter, mapMaybe, null)
import Internal.Prelude
import Debug.Trace

data Number = Number
    { num :: Int
    , surroundings :: [Char]
    }
    deriving stock (Show)

-- | X * Y
type Pos = (Int, Int)

type CharGrid = Map Pos Char

exampleData :: [String]
exampleData =
    [ "467..114.."
    , "...*......"
    , "..35..633."
    , "......#..."
    , "617*......"
    , ".....+.58."
    , "..592....."
    , "......755."
    , "...$.*...."
    , ".664.598.."
    ]

parseLineToMap :: (Int, String) -> CharGrid
parseLineToMap (y, line) = fromList $ mapFst (,y) $ zip [0 ..] line

parseInputToMap :: [String] -> CharGrid
parseInputToMap input = unions $ parseLineToMap <$> zip [0 ..] input

consumeDigit :: [String] -> Either (Char, [String]) [String]
consumeDigit ((d : restOfString) : restOfInput) | isNumber d = Left (d, restOfString : restOfInput)
consumeDigit input = Right input

surroundings :: CharGrid -> Pos -> [Char]
surroundings charGrid (posX, posY) =
    mapMaybe (charGrid !?) [(posX, posY - 1), (posX, posY + 1)]

fullSurroundings :: CharGrid -> Pos -> [Char]
fullSurroundings charGrid (posX, posY) =
    mapMaybe (charGrid !?) [(posX, posY - 1), (posX, posY), (posX, posY + 1)]

parseNumber :: CharGrid -> Maybe Pos -> [Pos] -> [Number]
parseNumber charGrid prevPos remainingPositions = do
    let oldSur = fullSurroundings charGrid <$> prevPos

    let fromGrid pos = charGrid ! pos

    let innerParseNumber :: [Pos] -> (String, [Char]) -> (Number, [Pos])
        innerParseNumber (currentPos : rest) (acc, sur) | isDigit $ fromGrid currentPos = do
            let newSur = surroundings charGrid currentPos
            innerParseNumber rest (acc <> [fromGrid currentPos], sur <> newSur)
        innerParseNumber [] (acc, sur) = (Number{num = read acc, surroundings = sur}, [])
        innerParseNumber (next : rest) (acc, sur) = do
            let nextSur = fullSurroundings charGrid next
            (Number{num = read acc, surroundings = sur <> nextSur}, rest)

    let (num, rest) = innerParseNumber remainingPositions ("", concat oldSur)
    num : parse charGrid rest

parse :: CharGrid -> [Pos] -> [Number]
parse _ [] = []
parse charGrid (first : remainingPositions) = innerParse first remainingPositions
  where
    fromGrid pos = charGrid ! pos

    innerParse :: Pos -> [Pos] -> [Number]
    -- This can really only happen to the first char
    innerParse prev (next : rest) | isDigit $ fromGrid prev = parseNumber charGrid Nothing (prev : next : rest)
    innerParse prev (next : rest) | isDigit $ fromGrid next = parseNumber charGrid (Just prev) (next : rest)
    innerParse _ (next : rest) = innerParse next rest
    innerParse _ [] = []

sumOfParts :: [Number] -> Int
sumOfParts [] = 0
sumOfParts (first : rest) = do
    let nonDots = filter (/= '.') first.surroundings
    if not $ null nonDots
        then first.num + sumOfParts rest
        else sumOfParts rest

main :: [String] -> String
main rawInputs = do
    let inputs = filter (/= "") rawInputs
    let charGrid = parseInputToMap inputs

    let dimensionsX = trace (show $ length inputs) $ length inputs
    let dimensionsY = trace (show $ length $ head inputs) $ length (head inputs)
    let positions = [(x, y) | y <- [0 .. dimensionsY - 1], x <- [0 .. dimensionsX - 1]]
    show $ sumOfParts $ parse charGrid positions
