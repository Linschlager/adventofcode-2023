module AdventOfCode.Day2 where

import Internal.Prelude

data Color = Blue | Red | Green
    deriving stock (Eq, Show, Ord)

type GameSet = [(Int, Color)]
type Game = [GameSet]

type FilterRestrictions = [(Int, Color)]

parseColor :: String -> Maybe (Int, Color)
parseColor (splitOn " " -> (read -> num) : "blue" : _) = Just (num, Blue)
parseColor (splitOn " " -> (read -> num) : "red" : _) = Just (num, Red)
parseColor (splitOn " " -> (read -> num) : "green" : _) = Just (num, Green)
parseColor _ = Nothing

parseGameSet :: String -> GameSet
parseGameSet (splitOn ", " -> input) = mapMaybe parseColor input

parseGame :: String -> Game
parseGame (splitOn "; " -> input) = parseGameSet <$> input

parseLine :: String -> Maybe (Int, Game)
parseLine (splitOn ": " -> (gameWithIndex : (parseGame -> gameData) : _)) = do
    let gameIndex = read $ betterStripPrefix "Game " gameWithIndex
    Just (gameIndex, gameData)
parseLine _ = Nothing

numColorInSet :: Color -> GameSet -> Int
numColorInSet color gameSet = do
    let x = find ((== color) . snd) gameSet
    case x of
        Just (num, _) -> num
        Nothing -> 0

gameSetMatchesRestrictions :: FilterRestrictions -> GameSet -> Bool
gameSetMatchesRestrictions restrictions gameSet =
    not $ any doesNotHaveEnoughCubes restrictions
  where
    doesNotHaveEnoughCubes :: (Int, Color) -> Bool
    doesNotHaveEnoughCubes (maxNum, color) = numColorInSet color gameSet > maxNum

gameMatchesRestrictions :: FilterRestrictions -> Game -> Bool
gameMatchesRestrictions restrictions = all $ gameSetMatchesRestrictions restrictions

minimumNumberOfCubes :: Game -> Int
minimumNumberOfCubes game = do
    let maxGreen = maxOfColor Green
    let maxRed = maxOfColor Red
    let maxBlue = maxOfColor Blue
    maxGreen * maxRed * maxBlue
  where
    maxOfColor :: Color -> Int
    maxOfColor color = fst $ maximum $ filter ((== color) . snd) $ concat game

exampleData :: [String]
exampleData =
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]

main :: [String] -> String
main inputLines = do
    let games = mapMaybe parseLine inputLines
    -- let restrictions :: FilterRestrictions = [(12, Red), (13, Green), (14, Blue)]
    -- let validGames = filter (gameMatchesRestrictions restrictions . snd) games

    show $ sum (minimumNumberOfCubes . snd <$> games)
