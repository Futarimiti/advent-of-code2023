{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Main (main, debug) where

import           Control.Monad  (filterM)
import           Data.Bifunctor (Bifunctor (first, second))
import           Data.Maybe     (mapMaybe)
import           Data.Vector    (Vector, (!?))
import qualified Data.Vector    as V
import           System.IO      (IOMode (WriteMode), hPutStrLn, openFile)

type Coords = (Int, Int)

data Pipe = Pipe { pipe :: Char
                 , pos  :: Coords
                 } deriving Show

-- for O(1) indexing
type System = Vector (Vector Char)

data Direction = N | E | S | W
  deriving (Eq, Show)

opposite :: Direction -> Direction
opposite = \case N -> S
                 S -> N
                 E -> W
                 W -> E

parseSystem :: String -> System
parseSystem = V.fromList . map V.fromList . lines

atMay :: System -> Coords -> Maybe Char
atMay sys (row, col) = do
  r <- sys !? row
  r !? col

findStart :: System -> Maybe Coords
findStart (V.indexed -> sys) = do
  (row, r) <- V.find (V.elem 'S' . snd) sys
  (col, 'S') <- V.find ((== 'S') . snd) (V.indexed r)
  return (row, col)

-- find the 2 openings for each pipe; not interested in order.
openings :: Char -> Maybe (Direction, Direction)
openings = \case '-' -> Just (E, W)
                 '|' -> Just (N, S)
                 'L' -> Just (N, E)
                 'J' -> Just (N, W)
                 '7' -> Just (W, S)
                 'F' -> Just (E, S)
                 _ -> Nothing

-- find the 2 direction where it is going from and to.
-- a pipe may not connect to 3 or more neighbours.
findConnected :: System -> Coords -> Maybe (Direction, Direction)
findConnected sys coords = do
  let surr = mapMaybe upon [N, E, S, W]
  [(a, _), (b, _)] <- filterM (uncurry $ flip connectsTo) surr
  return (a, b)
    where upon dir = do c <- sys `atMay` move1 dir coords
                        return (dir, c)
          ch `connectsTo` dir = do (x, y) <- openings ch
                                   return $ opposite dir `elem` [x, y]

-- start from given coords, move 1 pace to the given direction.
-- where will you be?
move1 :: Direction -> Coords -> Coords
move1 N = first $ subtract 1
move1 S = first (+1)
move1 W = second $ subtract 1
move1 E = second (+1)

-- you may be turned to another direction when passing through a pipe.
turn :: Char
     -> Direction        -- before
     -> Maybe Direction
turn '|' N = Just N
turn '|' S = Just S
turn '-' E = Just E
turn '-' W = Just W
turn 'F' N = Just E
turn 'F' W = Just S
turn 'L' W = Just N
turn 'L' S = Just E
turn 'J' S = Just W
turn 'J' E = Just N
turn '7' E = Just S
turn '7' N = Just W
turn _ _   = Nothing

-- take a full walk from start until once again standing on start,
-- returning the trail passed.
-- there could be two paths since the start point
-- has two openings; one is chosen at random.
walk :: System -> Maybe [Pipe]
walk sys = do start <- findStart sys
              (d, _) <- findConnected sys start
              -- first step must be manually made
              -- since we are starting with an 'S'
              let next = move1 d start
              ch <- sys `atMay` next
              ndir <- turn ch d
              trail <- walk' sys start next ndir []
              return $ Pipe 'S' start : trail

walk' :: System
      -> Coords     -- end
      -> Coords     -- curr
      -> Direction  -- where i am going next, not what brings me here
      -> [Pipe]     -- already been to
      -> Maybe [Pipe]
walk' sys end curr dir been
  | end == next = do thisch <- sys `atMay` curr
                     let this = Pipe thisch curr
                     return (this:been)
  | otherwise = do thisch <- sys `atMay` curr
                   nextch <- sys `atMay` next
                   ndir <- turn nextch dir
                   let this = Pipe thisch curr
                   walk' sys end next ndir (this:been)
  where next = move1 dir curr

debug :: IO ()
debug = do input <- readFile "input.txt"
           output <- openFile "trail.txt" WriteMode
           let sys = parseSystem input
           case walk sys of
             Just trail -> mapM_ (\Pipe{..} -> hPutStrLn output $ pipe : ' ' : show pos) trail
             Nothing    -> error "cannot walk"

main :: IO ()
main = do input <- readFile "input.txt"
          let sys = parseSystem input
          case walk sys of
            Just trail -> print (length trail `div` 2)
            _          -> error "Nothing"

