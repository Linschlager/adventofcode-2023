module AdventOfCode.Day5 where

import Data.List.Split (splitOn)
import Internal.Prelude

type Seed = Int
type SeedDefinition = [Int]

parseSeedRange :: SeedDefinition -> [Seed]
parseSeedRange (start : rangeLen : rest) = [start .. start + rangeLen - 1] ++ parseSeedRange rest
parseSeedRange _ = []

parseSeeds :: String -> [Seed]
parseSeeds inputLine = do
    let seeds = betterStripPrefix "seeds: " inputLine
    parseSeedRange $ read <$> splitOn " " seeds

data AlmanacMapEntry = AlmanacMapEntry
    { destRangeStart :: Int
    , srcRangeStart :: Int
    , rangeLength :: Int
    }
    deriving stock (Show)

type AlmanacCategory = [AlmanacMapEntry]

parseCategory :: [String] -> AlmanacCategory
parseCategory ls = do
    let actualLines = tail ls
    parseMapEntry <$> actualLines
  where
    parseMapEntry :: String -> AlmanacMapEntry
    parseMapEntry line = do
        let numbers = read <$> splitOn " " line
        AlmanacMapEntry
            { destRangeStart = head numbers
            , srcRangeStart = numbers !! 1
            , rangeLength = numbers !! 2
            }

type Almanac = [AlmanacCategory]

parseAlmanac :: [String] -> Almanac
parseAlmanac ls = parseCategory <$> splitOn [""] ls

passSeedThroughCategory :: Seed -> AlmanacCategory -> Int
passSeedThroughCategory seed category = do
    let maybeNext = find (\mapEntry -> seed >= mapEntry.srcRangeStart && seed <= mapEntry.srcRangeStart + mapEntry.rangeLength) category
    case maybeNext of
        Just next -> seed - next.srcRangeStart + next.destRangeStart
        Nothing -> seed

passSeedThroughAlmanac :: Almanac -> Seed -> Int
passSeedThroughAlmanac almanac seed = do
    let x = foldl passSeedThroughCategory seed almanac
    x

exampleData :: [String]
exampleData =
    [ "seeds: 79 14 55 13"
    , ""
    , "seed-to-soil map:"
    , "50 98 2"
    , "52 50 48"
    , ""
    , "soil-to-fertilizer map:"
    , "0 15 37"
    , "37 52 2"
    , "39 0 15"
    , ""
    , "fertilizer-to-water map:"
    , "49 53 8"
    , "0 11 42"
    , "42 0 7"
    , "57 7 4"
    , ""
    , "water-to-light map:"
    , "88 18 7"
    , "18 25 70"
    , ""
    , "light-to-temperature map:"
    , "45 77 23"
    , "81 45 19"
    , "68 64 13"
    , ""
    , "temperature-to-humidity map:"
    , "0 69 1"
    , "1 0 69"
    , ""
    , "humidity-to-location map:"
    , "60 56 37"
    , "56 93 4"
    ]

main :: [String] -> String
main (seedsStr : _ : categories) = do
    let almanac = parseAlmanac categories
    let seeds = parseSeeds seedsStr
    let locations = passSeedThroughAlmanac almanac <$> seeds
    -- show seeds
    show $ minimum locations
main _ = undefined
