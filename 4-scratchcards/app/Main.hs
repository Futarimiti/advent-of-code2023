{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Applicative          (Alternative (some))
import           Data.Char                    (isDigit)
import           Data.IntMap.Strict           (IntMap, alter, elems, empty,
                                               fromList, (!))
import           Data.List                    (find)
import           Data.Maybe                   (fromJust)
import           Text.ParserCombinators.ReadP

data Order = Win | My
  deriving (Eq, Show)

winningNumsBy :: Order  -- order by winning nums or my nums?
              -> [Int]  -- winning nums
              -> [Int]  -- my nums
              -> [Int]
winningNumsBy Win winning my = filter (`elem` winning) my
winningNumsBy My  winning my = filter (`elem` my) winning

points :: Int  -- winning num count
       -> Int
points 0 = 0
points 1 = 1
points n = 2 * points (n - 1)

cardPoints :: Card -> Int
cardPoints Card{..} = points $ length $ winningNumsBy Win winNums myNums

-- parsers

data Card = Card
  { cid     :: Int
  , winNums :: [Int]
  , myNums  :: [Int]
  } deriving (Eq, Show)

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

parseCard :: String -> Card
parseCard = fromJust . parseCardM

parseCardM :: String -> Maybe Card
parseCardM = fmap fst . find (null . snd) . readP_to_S card

int :: ReadP Int
int = do numStr <- munch1 isDigit
         return $ read numStr

-- main :: IO ()
-- main = do input <- readFile "input.txt"
--           let lined = lines input
--           let cards = map parseCard lined
--           let points = map cardPoints cards
--           print $ sum points

-- part 2

matches :: Card -> Int
matches Card{..} = length $ winningNumsBy My winNums myNums

-- which cards do I won after this one card?
-- may include non-existing cards
type CardID = Int
cardsWon :: Card -> [CardID]
cardsWon c@Card{..} = [cid + 1 .. cid + matches c]

type Pile = IntMap CardID

startingPile :: Pile
startingPile = fromList $ map (, 1) [1..219]

totalCards :: [Card] -> IntMap CardID
totalCards = foldl accum startingPile

accum :: IntMap CardID -> Card -> IntMap CardID
accum m c@Card{..} = foldl f m (cardsWon c)
  where f :: IntMap CardID -> CardID -> IntMap CardID
        f m i = alter (maybe (Just n) (Just . (+n))) i m
        n = m ! cid

main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let cards = map parseCard lined
          let total = totalCards cards
          let occurrences = elems total
          print $ sum occurrences
