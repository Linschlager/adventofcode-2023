{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day3 where

import Data.Char
import Data.Map.Strict hiding (filter, mapMaybe, null)
import Debug.Trace
import GHC.Exts (groupWith)
import Internal.Prelude

type Pos = (Int, Int)
type PositionedChar = (Char, Pos)

data Number = Number
    { num :: Int
    , surroundings :: [PositionedChar]
    }
    deriving stock (Show)

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

surroundings :: CharGrid -> Pos -> [PositionedChar]
surroundings charGrid (posX, posY) = do
    let positions = [(posX, posY - 1), (posX, posY + 1)]
    mapMaybe (\pos -> addFst pos <$> charGrid !? pos) positions

fullSurroundings :: CharGrid -> Pos -> [PositionedChar]
fullSurroundings charGrid (posX, posY) = do
    let positions = [(posX, posY - 1), (posX, posY), (posX, posY + 1)]
    mapMaybe (\pos -> addFst pos <$> charGrid !? pos) positions

parseNumber :: CharGrid -> Maybe Pos -> [Pos] -> [Number]
parseNumber charGrid prevPos remainingPositions = do
    let oldSur = fullSurroundings charGrid <$> prevPos

    let fromGrid pos = charGrid ! pos

    let innerParseNumber :: [Pos] -> (String, [PositionedChar]) -> (Number, [Pos])
        innerParseNumber (currentPos : rest) (acc, sur) | isDigit $ fromGrid currentPos = do
            let newSur = surroundings charGrid currentPos
            innerParseNumber rest (acc <> [fromGrid currentPos], sur <> newSur)
        innerParseNumber [] (acc, sur) = (Number{num = read acc, surroundings = sur}, [])
        innerParseNumber (next : rest) (acc, sur) = do
            let nextSur = fullSurroundings charGrid next
            (Number{num = read acc, surroundings = sur <> nextSur}, next : rest)

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
    let nonDots = filter ((/= '.') . fst) first.surroundings
    if not $ null nonDots
        then first.num + sumOfParts rest
        else sumOfParts rest

type Gear = (Int, Int)

toGearList :: [Number] -> [Gear]
toGearList numbers = do
    let gs = groupWith (snd . snd) $ concatMap toMappingList numbers
    listToTuple <$> filter ((== 2) . length) gs
  where
    toMappingList :: Number -> [(Int, PositionedChar)]
    toMappingList n = flip addFst n.num <$> filter (\(s, _) -> s == '*') n.surroundings

    listToTuple :: [(Int, PositionedChar)] -> Gear
    listToTuple [(a, _), (b, _)] = (a, b)
    listToTuple _ = undefined

productOfGear :: Gear -> Int
productOfGear (a, b) = a * b

main :: [String] -> String
main rawInputs = do
    let inputs = filter (/= "") rawInputs
    let charGrid = parseInputToMap inputs

    let dimensionsX = trace (show $ length inputs) $ length inputs
    let dimensionsY = trace (show $ length $ head inputs) $ length (head inputs)
    let positions = [(x, y) | y <- [0 .. dimensionsY - 1], x <- [0 .. dimensionsX - 1]]
    -- show $ sumOfParts $ parse charGrid positions
    show $ sum $ productOfGear <$> toGearList (parse charGrid positions)

-- show $ parse charGrid positions
