{-# LANGUAGE RecordWildCards #-}

module Main (main', main) where

import           Data.Bifunctor (Bifunctor (first, second))
import           Data.List      (elemIndices, sort, sortOn)
import           Data.Ord       (Down (Down))
import           Data.Set       (Set, delete, fromList, insert, member,
                                 toAscList, toDescList)
import           Data.Tuple     (swap)
import           System.IO      (IOMode (WriteMode), hClose, hPrint, openFile)

data Direction = North | South | East | West
  deriving (Eq, Show)

type Coords = (Int, Int)

data Dish = Dish { rounded :: Set Coords
                 , cubed   :: Set Coords
                 , height  :: Int
                 , width   :: Int
                 } deriving (Show, Eq)

instance Semigroup Dish where
  a <> b = Dish { rounded = r, cubed = c, height = h, width = w }
    where r = rounded a <> rounded b
          c = cubed a <> cubed b
          h = height a + height b
          w = max (width a) (width b)

instance Monoid Dish where
  mempty = Dish mempty mempty 0 0

-- Tilt the platform so that the round rocks all roll <North>
-- BUGGED:
tilt :: Direction -> Dish -> Dish
tilt dir dish = tilt' (toList $ rounded dish) dir dish
  where toList = case dir of
                   North -> toAscList
                   South -> toDescList
                   West  -> map swap . sort . map swap . toAscList
                   East  -> map swap . sortOn Down . map swap . toAscList

tilt' :: [Coords]  -- what's left to move?
      -> Direction -> Dish -> Dish
tilt' [] _ d = d
tilt' (c:cs) dir dish = tilt' cs dir after
  where after = afterMove dir dish c

afterMove :: Direction -> Dish -> Coords -> Dish
afterMove dir d old = d{rounded=r}
  where r = insert new $ delete old (rounded d)
        new = move dir d old

-- given all the context,
-- move a rock at given coords to the furthest
-- on a direction
-- until it reaches the edge
-- or run into another rock.
move :: Direction -> Dish -> Coords -> Coords
move dir dish old
  | newCoords `isOutOfBoundOf` dish = old
  | dish `hasRockAt` newCoords = old
  | otherwise = move dir dish newCoords
  where newCoords = nextCoords dir old

nextCoords :: Direction -> Coords -> Coords
nextCoords North = first $ subtract 1
nextCoords South = first (+1)
nextCoords West  = second $ subtract 1
nextCoords East  = second (+1)

isOutOfBoundOf :: Coords -> Dish -> Bool
isOutOfBoundOf (row, col) Dish{..} = row < 0 || row >= height || col < 0 || col >= width

hasRockAt :: Dish -> Coords -> Bool
d `hasRockAt` c = (d `byCoords` c) /= '.'

byCoords :: Dish -> Coords -> Char
byCoords Dish{..} c
  | c `member` rounded = 'O'
  | c `member` cubed = '#'
  | otherwise = '.'

-- total load on the <North> support beams
load :: Direction -> Dish -> Int
load North Dish{..} = foldr ((+) . load1) 0 rounded
  where load1 (row, _) = height - row
load _     _ = error "todo"

parseDish :: String -> Dish
parseDish = do lined <- lines
               let enum = zip [0..] lined
               let dishes = map (uncurry parseLine) enum
               return $ mconcat dishes

parseLine :: Int     -- curr line number
          -> String
          -> Dish
parseLine k str = Dish os pounds 1 (length str)
  where os = coords 'O'
        pounds = coords '#'
        coords c = fromList $ map (k,) $ elemIndices c str

main' :: IO ()
main' = do input <- readFile "test.txt"
           let dish = parseDish input
           let tilted = tilt North dish
           print $ load North tilted

-- part 2

tiltCycle :: Dish -> Dish
tiltCycle = tilt East . tilt South . tilt West . tilt North

-- try find the pattern yourself :)
main :: IO ()
main = do input <- readFile "input.txt"
          output <- openFile "output.txt" WriteMode
          let dish = parseDish input
          let tilteds = iterate tiltCycle dish
          let first1000 = map (load North) (take 1000 tilteds)
          let enum100 = zip [(0 :: Int)..] first1000
          mapM_ (hPrint output) enum100
          hClose output
