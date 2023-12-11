module AdventOfCode.Day6 where

import Internal.Prelude

data Game = Game { time :: Int, minDistance :: Int }
    deriving stock (Show)

parseSplitNumber :: String -> Int
parseSplitNumber str = innerParseSplitNumber str ""
    where
        innerParseSplitNumber :: String -> String -> Int
        innerParseSplitNumber [] acc = read $ reverse acc
        innerParseSplitNumber (h:rest) acc | isNumber h = innerParseSplitNumber rest (h:acc)
        innerParseSplitNumber (_:rest) acc = innerParseSplitNumber rest acc

parseGame :: [String] -> Game
parseGame ls = do
    let time = parseSplitNumber $ splitOn ":" (head ls) !! 1
    let minDistance = parseSplitNumber $ splitOn ":" (ls !! 1) !! 1
    Game {..}

exampleData :: [String]
exampleData =
    [ "Time:      7  15   30"
    , "Distance:  9  40  200"
    ]

-- | StartTime -> TotalTime -> Distance
formula :: Int -> Int -> Int
formula startTime totalTime = do
    let driveTime = totalTime - startTime
    let speed = startTime
    let distance = driveTime * speed
    distance

allTimes :: Game -> [Int]
allTimes game = [ formula time game.time | time <- [0..game.time] ]

winningTimes :: Game -> [Int]
winningTimes game = filter (> game.minDistance) $ allTimes game

main :: [String] -> String
main ls = do
    let game = parseGame ls
    let winningGame = winningTimes game
    show $ length winningGame

