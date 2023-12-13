{-# LANGUAGE ViewPatterns #-}

module Main (main', main) where

import           Data.List       (find, transpose, (!?))
import           Data.List.Split (splitOn)

type Pattern = [[Char]]

hasMirrorAt :: Pattern -> (Int, Int) -> Bool
hasMirrorAt cs (x, y) = case (cs !? x, cs !? y) of
  (Just a, Just b) -> a == b && hasMirrorAt cs (x - 1, y + 1)
  _                -> True

slits :: Pattern -> [(Int, Int)]
slits pat = [(y - 1, y) | y <- [1..upper]]
  where upper = length pat - 1

horiz :: Pattern -> Maybe (Int, Int)
horiz pat = find (hasMirrorAt pat) (slits pat)

vert :: Pattern -> Maybe (Int, Int)
vert = horiz . transpose

summarise :: Pattern -> Int
summarise (horiz -> Just (_, y)) = 100 * y
summarise (vert -> Just (_, y))  = y
summarise _                      = error "No mirror found"

main' :: IO ()
main' = do input <- readFile "input.txt"
           let patterns = splitOn [""] $ lines input
           let points = map summarise patterns
           print $ sum points

-- part 2

canBeFixed :: Pattern -> (Int, Int) -> Bool
canBeFixed cs (x, y) = case (cs !? x, cs !? y) of
  (Just a, Just b)
    | a == b -> canBeFixed cs (x - 1, y + 1)
    | diffBy1 a b -> hasMirrorAt cs (x - 1, y + 1)
  _ -> False

diffBy1 :: [Char] -> [Char] -> Bool
diffBy1 = diffBy1' False  -- found diff?
  where diffBy1' b [] [] = b
        diffBy1' False (x:xs) (y:ys) = diffBy1' (x /= y) xs ys
        diffBy1' True (x:xs) (y:ys)
          | x == y = diffBy1' True xs ys
          | otherwise = False
        diffBy1' _ _ _ = error "diffBy1': lists must be of equal length"

-- fix pattern, then check for mirror
horiz' :: Pattern -> Maybe (Int, Int)
horiz' pat = find (canBeFixed pat) (slits pat)

vert' :: Pattern -> Maybe (Int, Int)
vert' = horiz' . transpose

-- summarise' :: Pattern -> Int
summarise' :: Pattern -> Int
summarise' (horiz' -> Just (_, y)) = 100 * y
summarise' (vert' -> Just (_, y))  = y
summarise' _                       = error "No mirror found"

main :: IO ()
main = do input <- readFile "input.txt"
          let patterns = splitOn [""] $ lines input
          let points = map summarise' patterns
          print $ sum points
