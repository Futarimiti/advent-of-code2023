{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main', main) where

import           Data.Bifunctor (Bifunctor (bimap))
import           Data.Char      (digitToInt)
import           Data.List      (group, partition, sort, sortOn)
import           Data.Ord       (Down (Down))

data Card = N Int | T | J | Q | K | A
  deriving (Eq, Show, Ord)

readCard :: Char -> Card
readCard 'T' = T
readCard 'J' = J
readCard 'Q' = Q
readCard 'K' = K
readCard 'A' = A
readCard c   = N $ digitToInt c

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Show, Ord)

newtype Hand = Hand { getCards :: [Card] }
  deriving (Eq, Show)

readHand :: String -> Hand
readHand = Hand . map readCard

instance Ord Hand where
  compare h1@(Hand xs) h2@(Hand ys) = case compare (handTypeOf h1) (handTypeOf h2) of
    EQ -> compare xs ys
    b  -> b

handTypeOf :: Hand -> HandType
handTypeOf (Hand cards)
  | eq 5 = FiveOfAKind
  | eq 4 = FourOfAKind
  | eq 3 && eq 2 = FullHouse
  | eq 3 = ThreeOfAKind
  | twoPair = TwoPair
  | eq 2 = OnePair
  | otherwise = HighCard
  where eq n = (any ((== n) . length) . group . sort) cards
        twoPair = ((== 2) . length) . filter ((== 2) . length) . group . sort $ cards

type Bid = Int
readHandBids :: String -> (Hand, Bid)
readHandBids line = bimap readHand read $ (\[a, b] -> (a, b)) $ words line

main' :: IO ()
main' = do input <- readFile "input.txt"
           let lined = lines input
           let handsNbids = map readHandBids lined
           let rankedBids = map snd $ sortOn fst handsNbids
           let enumedBids = zip [1..] rankedBids
           let winnings = map (uncurry (*)) enumedBids
           print $ sum winnings

-- part 2

data Card' = J' | N' Int | T' | Q' | K' | A'
  deriving (Eq, Show, Ord)

readCard' :: Char -> Card'
readCard' = \case 'T' -> T'
                  'J' -> J'
                  'Q' -> Q'
                  'K' -> K'
                  'A' -> A'
                  c   -> N' $ digitToInt c

newtype Hand' = Hand' { getCards' :: [Card'] }
  deriving (Eq, Show)

readHand' :: String -> Hand'
readHand' = Hand' . map readCard'

instance Ord Hand' where
  compare h1@(Hand' xs) h2@(Hand' ys) = case compare (handTypeOf' h1) (handTypeOf' h2) of
    EQ -> compare xs ys
    b  -> b

-- considering J as wildcard
-- sorry, cannot find a cleaner way
handTypeOf' :: Hand' -> HandType
handTypeOf' (Hand' cards) = case others of
                              [] -> FiveOfAKind
                              _ -> case highest + js of
                                      5 -> FiveOfAKind
                                      4 -> FourOfAKind
                                      3 -> case sndHighest of
                                            2 -> FullHouse
                                            _ -> ThreeOfAKind
                                      2 -> case sndHighest of
                                            2 -> TwoPair
                                            _ -> OnePair
                                      _ -> HighCard
  where (length -> js, others) = partition (== J') cards
        amounts = map length . group $ sort others
        highest:rest = sortOn Down amounts
        sndHighest:_ = rest

readHandBids' :: String -> (Hand', Bid)
readHandBids' line = bimap readHand' read $ (\[a, b] -> (a, b)) $ words line

main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let handsNbids = map readHandBids' lined
          let rankedBids = map snd $ sortOn fst handsNbids
          let enumedBids = zip [1..] rankedBids
          let winnings = map (uncurry (*)) enumedBids
          print $ sum winnings
