{-# LANGUAGE ViewPatterns #-}

module Main (main, main') where

import           Algorithm.Search     (dijkstraAssoc)
import           Control.Monad.Reader (Reader, asks, runReader)
import           Data.Char            (digitToInt)
import           Data.List            (delete)
import           Data.Map             ((!?))
import qualified Data.Map             as M
import           Prelude              hiding (map)

type Coords = (Int, Int)
type Cost = Int

data Map = Map { height :: Int
               , width  :: Int
               , map    :: M.Map Coords Cost
               } deriving (Show)

data Direction = N | S | E | W
  deriving (Show, Eq, Enum, Ord)

-- including number of blocks you have travelled
-- in a single direction
type Node = (Direction, Int, Coords)

nextCoords :: Direction -> Coords -> Coords
nextCoords N (x, y) = (x - 1, y)
nextCoords S (x, y) = (x + 1, y)
nextCoords E (x, y) = (x, y + 1)
nextCoords W (x, y) = (x, y - 1)

-- find neighbours of a node that are reachable
-- without taking > 3 steps in a single direction
neighbourNodes :: Node -> [Node]
neighbourNodes (dir, n, coords)
  | n == 3 = others
  | otherwise = continue : others
  where continue = (dir, n + 1, nextBy dir)
        others = (\d -> (d, 1, nextBy d)) <$> otherDirs
        nextBy d = nextCoords d coords
        otherDirs = delete dir [N .. W]

neighbours :: Reader Map (Node -> [(Node, Cost)])
neighbours = do m <- asks map
                return $ \node -> let nextNodes = neighbourNodes node
                                  in [(n, cost) | n@(_, _, c) <- nextNodes, Just cost <- [m !? c]]

arrived :: Reader Map (Node -> Bool)
arrived = do h <- asks height
             w <- asks width
             return $ \(_, _, (x, y)) -> x == h - 1 && y == w - 1

start :: Reader Map Node
start = return (N, 0, (0,0))

parseMap :: String -> Map
parseMap txt = let m = foldr1 M.union $ zipWith nodesOn [0..] (lines txt)
                in Map {height=length $ lines txt, width=length $ head $ lines txt, map=m}

nodesOn :: Int  -- row
        -> String
        -> M.Map Coords Cost
nodesOn row (fmap digitToInt -> ints) = M.fromList $ zip coords's ints
  where coords's = fmap (row,) [0..]

findPath :: Reader Map (Maybe (Cost, [Node]))
findPath = do initial <- start
              next <- neighbours
              found <- arrived
              return $ dijkstraAssoc next found initial

main' :: IO ()
main' = do input <- readFile "input.txt"
           let m = parseMap input
           let path = runReader findPath m
           print path

-- part 2

main :: IO ()
main = do input <- readFile "input.txt"
          let m = parseMap input
          let path = runReader findPath' m
          print path

findPath' :: Reader Map (Maybe (Cost, [Node]))
findPath' = do initial <- start
               next <- neighbours'
               found <- arrived
               return $ dijkstraAssoc next found initial

neighbours' :: Reader Map (Node -> [(Node, Cost)])
neighbours' = do m <- asks map
                 return $ \node -> [(n, cost) | n@(_, _, c) <- neighbourNodes' node, Just cost <- [m !? c]]

-- alt rules: in one direction, you must move >= 4 and <= 10 blocks
neighbourNodes' :: Node -> [Node]
neighbourNodes' (dir, n, coords)
  | n == 10 = others
  | n < 4 && n > 0 = [continue]
  | otherwise = continue : others
  where continue = (dir, n + 1, nextBy dir)
        others = (\d -> (d, 1, nextBy d)) <$> otherDirs
        nextBy d = nextCoords d coords
        otherDirs = delete dir [N .. W]
