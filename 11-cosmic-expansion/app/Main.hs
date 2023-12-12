{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.List (elemIndices, tails, (\\))

type Step = Int
type Coords = (Int, Int)  -- row, col
type Galaxies = [Coords]

data Image = Image { galaxies :: Galaxies
                   , width    :: Int
                   , height   :: Int
                   } deriving (Show, Eq)

parseImage :: String -> Image
parseImage s = let enumerated = zip [0..] (lines s)
                in Image{ galaxies = [(row, col) | (row, line) <- enumerated, col <- parseLine line]
                        , width = length (head (lines s))
                        , height = length (lines s)
                        }

parseLine :: String -> [Int]
parseLine = elemIndices '#'

galaxyPairs :: Image -> [(Coords, Coords)]
galaxyPairs Image{galaxies} = [(x, y) | (x:rest) <- tails galaxies, y <- rest]

expand :: Image -> Image
expand i@Image{..} = expandCols emptyCols $ expandRows emptyRows i
  where -- [*, *, <empty>, *, *] -> [*, *, <empty>, <empty>, <empty>, *, *]
        expandCols :: [Int] -> Image -> Image
        expandCols xs i = foldr expandCol i xs

        expandCol :: Int -> Image -> Image
        -- part 1
        -- expandCol n i@Image{..} = Image{galaxies=fmap (\(row, col) -> if col > n then (row, col + 1) else (row, col)) galaxies, width=width+1, height=height}
        -- part 2
        expandCol n i@Image{..} = Image{galaxies=fmap (\(row, col) -> if col > n then (row, col + million) else (row, col)) galaxies, width=width+million, height=height}

        expandRows :: [Int] -> Image -> Image
        expandRows xs i = foldr expandRow i xs

        expandRow :: Int -> Image -> Image
        -- part 1
        -- expandRow n i@Image{..} = Image{galaxies=fmap (\(row, col) -> if row > n then (row + 1, col) else (row, col)) galaxies, width=width, height=height+1}
        -- part 2
        expandRow n i@Image{..} = Image{galaxies=fmap (\(row, col) -> if row > n then (row + million, col) else (row, col)) galaxies, width=width, height=height+million}

        million = 1000000 - 1  -- ;p

        -- [8,14,20,47,48,51,92,93,120]
        emptyRows :: [Int]
        emptyRows = [0 .. width - 1] \\ map fst galaxies

        -- [30,33,50,57,66,74,75,77,80,93,121,135]
        emptyCols :: [Int]
        emptyCols = [0 .. height - 1] \\ map snd galaxies


distBtw :: Coords -> Coords -> Step
distBtw (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do input <- readFile "input.txt"
          let image = parseImage input
          let expanded = expand image
          let gPairs = galaxyPairs expanded
          let dists = map (uncurry distBtw) gPairs
          let total = sum dists
          print total

