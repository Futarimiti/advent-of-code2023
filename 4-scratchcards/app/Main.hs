{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Applicative          (Alternative (some))
import           Data.Char                    (isDigit)
import           Data.IntMap                  (IntMap, alter, elems, empty,
                                               fromList, (!))
import           Data.List                    (find)
import           Data.Maybe                   (fromJust)
import           Text.ParserCombinators.ReadP

data Card = Card
  { cid     :: Int
  , winNums :: [Int]
  , myNums  :: [Int]
  } deriving (Eq, Show)

winningNums :: [Int]  -- winning nums
            -> [Int]  -- my nums
            -> [Int]
winningNums = filter . flip elem

matches :: Card -> Int
matches Card{..} = length $ winningNums winNums myNums

points :: Int  -- winning num count
       -> Int
points 0 = 0
points 1 = 1
points n = 2 * points (n - 1)

cardPoints :: Card -> Int
cardPoints = points . matches

-- parsers

card :: ReadP Card
card = do _ <- string "Card"
          skipSpaces
          cid <- int
          _ <- char ':'
          skipSpaces
          wins <- sepBy1 int skipSpaces
          _ <- string " | "
          skipSpaces
          my <- sepBy1 int skipSpaces
          return $ Card cid wins my
  where int = read <$> munch1 isDigit

parseCard :: String -> Card
parseCard = fst . fromJust . find (null . snd) . readP_to_S card


-- main :: IO ()
-- main = do input <- readFile "input.txt"
--           let lined = lines input
--           let cards = map parseCard lined
--           let points = map cardPoints cards
--           print $ sum points

-- part 2

-- which cards do I won after this one card?
-- may include non-existing cards
type CardID = Int
cardsWon :: Card -> [CardID]
cardsWon c@Card{..} = [cid + 1 .. cid + matches c]

type Pile = IntMap CardID

startingPile :: Pile
startingPile = fromList $ map (, 1) [1..219]

totalCards :: [Card] -> Pile
totalCards = foldl accum startingPile

accum :: Pile -> Card -> Pile
accum m c@Card{..} = foldl f m (cardsWon c)
  where f :: Pile -> CardID -> Pile
        f m i = alter (maybe (Just n) (Just . (+n))) i m
        n = m ! cid

main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let cards = map parseCard lined
          let total = totalCards cards
          let occurrences = elems total
          print $ sum occurrences
