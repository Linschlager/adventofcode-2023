{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day3 where

import Data.Char
import Data.Map.Strict hiding (filter, mapMaybe, null)
import Debug.Trace
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
    let positions =[(posX, posY - 1), (posX, posY + 1)]
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

findGears :: [Number] -> [(Int, Int)]
findGears numbers = do
    let x = toList $ innerFindGears numbers empty
    let y = snd <$> filter (\((c, _), nums) -> c == '*' && length nums == 2) x
    listToTuple <$> y

    where
        listToTuple :: [a] -> (a, a)
        listToTuple [f, s] = (f, s)
        listToTuple _ = undefined

        innerFindGears :: [Number] -> Map PositionedChar [Int] -> Map PositionedChar [Int]
        innerFindGears [] m = m
        innerFindGears allNumbers m = do
            let numSur = fmap (\n -> (n.surroundings, n.num)) allNumbers
            let func = innerInnerFindGears empty <$> (fst <$> numSur) <*> (snd <$> numSur)
            unionsWith (<>) func

            where
                innerInnerFindGears :: Map PositionedChar [Int] -> [PositionedChar] -> Int -> Map PositionedChar [Int]
                innerInnerFindGears innerMap (firstSur:surRest) n | fst firstSur == '*' = do
                    let newM = case m !? firstSur of
                            Just entry -> Data.Map.Strict.insert firstSur (n:entry) innerMap
                            Nothing -> Data.Map.Strict.insert firstSur [n] innerMap
                    innerInnerFindGears newM surRest n
                innerInnerFindGears innerMap _ _ = innerMap

main :: [String] -> String
main rawInputs = do
    let inputs = filter (/= "") rawInputs
    let charGrid = parseInputToMap inputs

    let dimensionsX = trace (show $ length inputs) $ length inputs
    let dimensionsY = trace (show $ length $ head inputs) $ length (head inputs)
    let positions = [(x, y) | y <- [0 .. dimensionsY - 1], x <- [0 .. dimensionsX - 1]]
    -- show $ sumOfParts $ parse charGrid positions
    show $ findGears $ parse charGrid positions
-- show $ parse charGrid positions
