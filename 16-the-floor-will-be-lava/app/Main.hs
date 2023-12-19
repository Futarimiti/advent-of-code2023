{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens         (makeLenses, set, view)
import           Control.Monad.Reader (Reader, asks, runReader)
import           Data.Bifunctor       (first, second)
import           Data.Map             (Map, insert, unions, (!?))
import           Data.Set             (Set)
import qualified Data.Set             as S

type Coords = (Int, Int)

data Direction = N | E | S | W
  deriving (Show, Eq, Ord)

data Beam = Beam
  { _source    :: Coords
  , _direction :: Direction
  } deriving (Show, Eq, Ord)

makeLenses ''Beam

data Mirror = Ver | Hor | Fwd | Bwd
  deriving (Show, Eq)

type Contraption = Map Coords Mirror

data Image = Image
  { _width       :: Int
  , _height      :: Int
  , _contraption :: Contraption
  } deriving (Show, Eq)

makeLenses ''Image

main :: IO ()
main = do input <- readFile "input.txt"
          Just image <- return $ parseImage input
          let start = Beam (0, 0) E
          let t = runReader (trail start) image
          print $ length t

parseImage :: String -> Maybe Image

-- tiles that the beam passes through.
-- a beam stops when hits into a wall,
-- or into a tile that has been visited in the same direction before
trail :: Beam -> Reader Image (Set Coords)

--- impl {{{

parseImage s = Image w h <$> maybeC
  where lined = lines s
        w = length . head $ lined
        h = length lined
        maybeC = parseContraption s

        parseContraption txt = do
          cs <- sequence contraptions
          return $ unions cs
            where contraptions = zipWith (\row line -> parseLine (row, 0) line mempty) [0..] (lines txt)

        parseLine _ "" acc = Just acc
        parseLine (row, col) ('.':cs) acc = parseLine (row, col + 1) cs acc
        parseLine (row, col) (c:cs) acc = do
          m <- parseMirror c
          let inserted = insert (row, col) m acc
          parseLine (row, col + 1) cs inserted

        parseMirror = \case '|' -> Just Ver
                            '-' -> Just Hor
                            '/' -> Just Fwd
                            '\\' -> Just Bwd
                            _ -> Nothing

deflect :: Mirror -> Direction -> Either (Direction, Direction) Direction
deflect Ver N = Right N
deflect Ver S = Right S
deflect Ver _ = Left (N, S)
deflect Hor E = Right E
deflect Hor W = Right W
deflect Hor _ = Left (W, E)
deflect Fwd N = Right E
deflect Fwd S = Right W
deflect Fwd W = Right S
deflect Fwd E = Right N
deflect Bwd N = Right W
deflect Bwd S = Right E
deflect Bwd W = Right N
deflect Bwd E = Right S

nextSource :: Beam -> Contraption -> Either (Beam, Beam) Beam
nextSource beam contrap
  | Just mirror <- contrap !? src, Right d <- deflect mirror dir = Right $ set direction d $ set source (moveBy d) beam
  | Just mirror <- contrap !? src, Left (d1, d2) <- deflect mirror dir = Left (set direction d1 $ set source (moveBy d1) beam, set direction d2 $ set source (moveBy d2) beam)
  | otherwise = Right $ set source (moveBy dir) beam
  where moveBy d = move1 d src
        move1 N = first (subtract 1)
        move1 S = first (+ 1)
        move1 E = second (+ 1)
        move1 W = second (subtract 1)
        src = view source beam
        dir = view direction beam

trail beam = do t <- trail' mempty beam
                return $ S.map (view source) t

trail' :: Set Beam -> Beam -> Reader Image (Set Beam)
trail' acc beam | beam `elem` acc = return acc
trail' acc beam = do contrap <- asks $ view contraption
                     h <- asks $ view height
                     w <- asks $ view width
                     let (row, col) = view source beam
                     let outOfBounds = row < 0 || col < 0 || row >= h || col >= w
                     let next = nextSource beam contrap
                     if | outOfBounds -> return acc
                        | Right b <- next -> trail' (S.insert beam acc) b
                        | Left (b1, b2) <- next -> do res1 <- trail' (S.insert beam acc) b1
                                                      trail' res1 b2

--- }}}
