-- https://adventofcode.com/2023/day/3#part2
-- PART 2 ONLY

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where
import           Control.Monad
import           Data.Char
import           Safe          (atMay)

type Enumerated a = [(Int, a)]
type Pos = (Int, Int)  -- (row, col)
type Range = (Int, (Int, Int))  -- (row, (col_start, col_end))

enumerate :: [a] -> Enumerated a
enumerate = zip [0..]

-- find position of *
findStars :: [String] -> [Pos]
findStars rows = [(r, c) | (r, row) <- enumerate rows , (c, '*') <- enumerate row]

-- since this looks up ranges without context,
-- the ranges may be out of bounds or overlap
-- You should exclusively use `inRange` to avoid this
findRanges :: [Pos] -> [[Range]]
findRanges = map findRange

findRange :: Pos -> [Range]
findRange (0, 0) = [ (0, (0, 1))
                   , (1, (0, 1))
                   ]
findRange (0, c) = [ (0, (c-1, c+1))
                   , (1, (c-1, c+1))
                   ]
findRange (r, 0) = [ (r-1, (0, 1))
                   , (r, (0, 1))
                   , (r+1, (0, 1))
                   ]
findRange (r, c) = [ (r-1, (c-1, c+1))
                   , (r, (c-1, c+1))
                   , (r+1, (c-1, c+1))
                   ]

overlapping :: (Int, Int) -> (Int, Int) -> Bool
overlapping (a1, a2) (b1, b2) = a1 <= b2 && b1 <= a2

-- find numbers that part of them falling in the range
findNums :: [String]  -- context
         -> Range
         -> [Int]
findNums xs (r, (c1, c2)) = case atMay xs r of
  Nothing  -> []
  Just row -> findNum1 (enumerate row) (c1, c2)

findNum1 :: Enumerated Char -> (Int, Int) -> [Int]
findNum1 [] _ = []
findNum1 str@((idx, c):cs) range@(_, end)
  | idx > end = []
  | isDigit c, numPos `overlapping` range = read (snd <$> num) : findNum1 rest range
  | otherwise = findNum1 cs range
  where (num, rest) = span (isDigit . snd) str
        numPos = (fst $ head num, fst $ last num)

filterGears :: [[Int]] -> [(Int, Int)]
filterGears = map (\[x, y] -> (x, y)) . filter ((== 2) . length)

sumRatios :: [String] -> Int
sumRatios s = let stars = findStars s
                  ranges = findRanges stars
                  nums = map (join . map (findNums s)) ranges
                  gears = filterGears nums
               in sum $ uncurry (*) <$> gears

main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let total = sumRatios lined
          print total
