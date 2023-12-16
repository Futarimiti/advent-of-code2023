-- https://adventofcode.com/2023/day/2
module Main (main) where

import           Control.Applicative          (Alternative (some, (<|>)))
import           Data.Char                    (isDigit, toUpper)
import           Text.ParserCombinators.ReadP

data Cubes = Cubes { red   :: Integer
                   , blue  :: Integer
                   , green :: Integer
                   } deriving (Show, Eq)

instance Semigroup Cubes where
  (<>) = addBalls

data Colour = Red | Green | Blue deriving (Show, Eq, Read)

main' :: IO ()
main' = do input <- readFile "input.txt"
           let lined = lines input
           let records = map parseLine' lined
           let possibleRecords = filter (all possible . snd) records
           let possibleIDs = map fst possibleRecords
           print $ sum possibleIDs


bag :: Cubes
bag = Cubes {red=12, green=13, blue=14}

possible :: Cubes -> Bool
possible (Cubes r g b) = r <= red bag && g <= green bag && b <= blue bag

type ID = Integer

int :: ReadP Integer
int = do digits <- some $ satisfy isDigit
         return $ read digits

-- parse 1 line
-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
parseLine :: ReadP (ID, [Cubes])
parseLine = do _ <- string "Game "
               id' <- int
               _ <- string ": "
               balls <- parseBalls
               return (id', balls)

parseLine' :: String -> (ID, [Cubes])
parseLine' s = let results = readP_to_S parseLine s
                   results' = filter (null . snd) results
                   res = fst $ head results'
                in res

parseBalls :: ReadP [Cubes]
parseBalls = sepBy1 oneTake (string "; ")

-- 3 blue, 4 red
oneTake :: ReadP Cubes
oneTake = do groups <- sepBy1 oneGroup (string ", ")
             return $ foldr1 addBalls (map makeBall groups)

addBalls :: Cubes -> Cubes -> Cubes
addBalls (Cubes r1 g1 b1) (Cubes r2 g2 b2) = Cubes (r1+r2) (g1+g2) (b1+b2)

makeBall :: (Colour, Integer) -> Cubes
makeBall (Red, n)   = Cubes n 0 0
makeBall (Green, n) = Cubes 0 n 0
makeBall (Blue, n)  = Cubes 0 0 n

oneGroup :: ReadP (Colour, Integer)
oneGroup = do n <- int
              _ <- string " "
              c <- colour
              return (c, n)

colour :: ReadP Colour
colour = do (c:cs) <- string "blue" <|> string "red" <|> string "green"
            return $ read (toUpper c : cs)

-- part 2

main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let takes = map (snd . parseLine') lined
          let maxes = map byMax takes
          let powers = map power maxes
          print $ sum powers

-- 3,4,1 2,6,2 -> 3,6,2
byMax :: [Cubes] -> Cubes
byMax = foldr (\(Cubes a b c) (Cubes x y z) -> Cubes (max a x) (max b y) (max c z)) (Cubes 0 0 0)

power :: Cubes -> Integer
power (Cubes x y z) = x * y * z
