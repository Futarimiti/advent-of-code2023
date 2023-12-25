{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main, debug) where

import           Control.Monad (join)
import           Data.Char     (digitToInt)
import           Data.DList    (DList)
import qualified Data.DList    as D
import           Data.Function (on)
import           Data.List     (groupBy, sortOn)
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Tuple    (swap)

-- x and y coords
type Coords = (Int, Int)

data Direction = R | D | L | U
  deriving (Show, Read)

type Direction2 = (Direction, Direction)

type Hex = String  -- including #

data Step = Step Direction Int Hex
  deriving (Show, Read)

type Plan = [Step]

parseStep :: String -> Step
parseStep line = let [dir, num, bracedHex] = words line
                  in Step (read dir) (read num) (init $ tail bracedHex)

parsePlan :: String -> Plan
parsePlan = map parseStep . lines

-- get the outline of trenches coords as digged from plan
-- having let the origin be (0,0)
outlineTrail :: Plan -> [Coords]
outlineTrail = D.toList . go mempty (0,0)
  where
    go :: DList Coords -> Coords -> Plan -> DList Coords
    go acc _ [] = acc
    go acc curr (step:steps) = let ts = trenches1 curr step
                                in go (acc <> D.fromList ts) (last ts) steps

    -- get trenches from a single step given the current position (not included)
    trenches1 :: Coords -> Step -> [Coords]
    trenches1 coords (Step dir 1 _) = [move1By dir coords]
    trenches1 coords (Step dir n _) = let newCoords = move1By dir coords
                                       in newCoords : trenches1 newCoords (Step dir (n-1) undefined)

    -- ;)
    move1By :: Direction -> Coords -> Coords
    move1By dir (x,y) = case dir of
                          R -> (x+1, y)
                          D -> (x, y-1)
                          L -> (x-1, y)
                          U -> (x, y+1)

-- expect consecutive coords
turn :: Coords -> Coords -> Direction
turn (x1, y1) (x2, y2)
  | x1 == x2 = case y1 - y2 of
                 1  -> D
                 -1 -> U
                 _  -> error "non-consecutive coords"
  | y1 == y2 = case x1 - x2 of
                 1  -> L
                 -1 -> R
                 _  -> error "non-consecutive coords"
  | otherwise = error "non-consecutive coords"

-- expect 3 consecutive coords
turnTo :: Coords -> Coords -> Coords -> Direction2
turnTo c1 c2 c3 = (turn c1 c2, turn c2 c3)

-- generate infinite triples of consecutive 3 elements
every3 :: [a] -> [(a, a, a)]
every3 = every3' . cycle
  where every3' :: [a] -> [(a, a, a)]
        every3' ~(a:b:c:xs) = (a,b,c) : every3' (b:c:xs)

addDirections :: [Coords] -> [(Direction2, Coords)]
addDirections [] = error "empty trail"
addDirections trail@(t:ts) = zip directions (ts ++ [t])
  where directions = (\(a, b, c) -> turnTo a b c) <$> every3 trail

-- get coords of blocks inside the outline
inner :: [Coords] -> Set Coords
inner trail = S.fromList trail `S.union` S.fromList enclosed
  where directedCoords :: [(Direction2, Coords)]
        directedCoords = addDirections trail

        -- outline wrenches on each row (fst)
        rows :: [(Int, [(Direction2, Int)])]
        rows = map f $ groupBy ((==) `on` (snd . snd)) $ sortOn (swap . snd) directedCoords
          where f xs@((_, (_, y)):_) = (y, map (fmap fst) xs)
                f xs                 = error $ "impossible: " ++ show xs

        -- variant of @rows@ with only the down directions filtered out
        downRows :: [(Int, [Int])]
        downRows = map (fmap (map snd . filter (goDown . fst))) rows
          where goDown :: Direction2 -> Bool
                goDown (_, D) = True
                goDown (U, _) = True
                goDown _      = False

        -- get the coords in between two x coord's, given the row
        coordsBtw :: Int -> Int -> Int -> [Coords]
        coordsBtw y x1 x2 = (,y) <$> [x1..x2]

        coordsRow :: Int -> [Int] -> [Coords]
        coordsRow y xs = pairs xs >>= uncurry (coordsBtw y)

        -- coordsRow applied to downRows
        enclosed :: [Coords]
        enclosed = join $ map (uncurry coordsRow) downRows

-- get all pairs of a list
-- error for odd length
pairs :: [a] -> [(a, a)]
pairs []       = []
pairs (x:y:xs) = (x, y) : pairs xs
pairs _        = error "odd length"

countInner :: [Coords] -> Int
countInner = length . inner

debug :: IO ()
debug = do input <- readFile "test.txt"
           let plan = parsePlan input
           let ts = outlineTrail plan
           mapM_ print $ inner ts
           print $ countInner ts

-- part 1

main :: IO ()
main = do input <- readFile "input.txt"
          let plan = parsePlan input
          let ts = outlineTrail plan
          let interior = countInner ts
          print interior
