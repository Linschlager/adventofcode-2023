module AdventOfCode.Day4 where

import Data.List.Split
import Internal.Prelude

data Card = Card {winningNumbers :: [Int], givenNumbers :: [Int]}
    deriving stock (Show)

parseCard :: String -> Card
parseCard line = do
    let numbers = splitOn "|" $ splitOn ":" line !! 1
    let winningNumbers :: [Int]
        winningNumbers = read <$> filter (not . null) (splitOn " " (head numbers))
    let givenNumbers :: [Int]
        givenNumbers = read <$> filter (not . null) (splitOn " " (numbers !! 1))
    Card{..}

scoreCard :: Card -> Int
scoreCard card = do
    let x = flip elem card.winningNumbers <$> card.givenNumbers
    length $ filter id x

mergeLists :: (Num a) => [a] -> [a] -> [a]
mergeLists [] x = x
mergeLists x [] = x
mergeLists (a : aRest) (b : bRest) = (a + b) : mergeLists aRest bRest

processCards :: [Card] -> Int
processCards cards = innerProcessCards cards $ replicate (length cards) 1
  where
    innerProcessCards :: [Card] -> [Int] -> Int
    innerProcessCards [] _ = 0
    innerProcessCards _ [] = 0
    innerProcessCards (card : rest) (numCards : numOtherCards) = do
        let numNextCards = scoreCard card
        let newNumOtherCards = mergeLists numOtherCards $ replicate numNextCards numCards
        let totalNumCards = numCards * numNextCards
        1 + totalNumCards + innerProcessCards rest newNumOtherCards

exampleData :: [String]
exampleData =
    [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ]

main :: [String] -> String
main ls = show $ processCards $ parseCard <$> ls
