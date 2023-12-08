{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- https://adventofcode.com/2023/day/5
module Main (main) where

import           Control.Monad                (void)
import           Data.Char                    (isAlpha, isDigit)
import           Data.Foldable                (find)
import           Data.List                    (sortOn)
import           Text.ParserCombinators.ReadP hiding (get)

main' :: IO ()
main' = do input <- readFile "input.txt"
           let Just a = parseAlmanac input
           let locs = fmap (locationBySeed a) (seeds a)
           print $ minimum locs

locationBySeed :: Almanac
               -> Int  -- seed
               -> Int
locationBySeed Almanac{..} seed = let soil = get seed seedToSoil
                                      fertilizer = get soil soilToFertilizer
                                      water = get fertilizer fertilizerToWater
                                      light = get water waterToLight
                                      temperature = get light lightToTemperature
                                      humidity = get temperature temperatureToHumidity
                                   in get humidity humidityToLocation

-- parsers

data Almanac = Almanac { seeds                 :: [Int]
                       , seedToSoil            :: Mapping
                       , soilToFertilizer      :: Mapping
                       , fertilizerToWater     :: Mapping
                       , waterToLight          :: Mapping
                       , lightToTemperature    :: Mapping
                       , temperatureToHumidity :: Mapping
                       , humidityToLocation    :: Mapping
                       } deriving Show

parseAlmanac :: String -> Maybe Almanac
parseAlmanac = fmap fst . find (null . snd) . readP_to_S almanac

-- the whole file
almanac :: ReadP Almanac
almanac = do s <- seedsP
             blankLine
             [ ("seed", "soil", st)
               , ("soil", "fertilizer", sf)
               , ("fertilizer", "water", fw)
               , ("water", "light", wl)
               , ("light", "temperature", lt)
               , ("temperature", "humidity", th)
               , ("humidity", "location", hl)
               ] <- sepBy1 mapping blankLine
             newline
             eof
             return $ Almanac s st sf fw wl lt th hl

-- "seeds: 79 14 55 13" -> [79, 14, 55, 13]
seedsP :: ReadP [Int]
seedsP = do _ <- string "seeds: "
            nstr <- sepBy1 (munch1 isDigit) skipSpaces
            return $ fmap read nstr

-- "seed-to-soil map:
-- 50 98 2
-- 52 50 48" -> ("seed", "soil", <Mapping>)
mapping :: ReadP (String, String, Mapping)
mapping = do (from, to) <- relation
             newline
             ms <- maps
             return (from, to, ms)

-- "50 98 2
-- 52 50 48" -> <Mapping>
maps :: ReadP Mapping
maps = do triplets <- sepBy1 tripleInt newline
          return $ makeMapping triplets

-- "52 50 48" -> (52, 50, 48)
tripleInt :: ReadP (Int, Int, Int)
tripleInt = do [a, b, c] <- map read <$> sepBy1 (munch1 isDigit) skipSpaces
               return (a, b, c)

newline :: ReadP ()
newline = void $ char '\n'

blankLine :: ReadP ()
blankLine = newline >> newline

-- "light-to-temperature map:" -> ("light", "temperature")
relation :: ReadP (String, String)
relation = do from <- munch1 isAlpha
              _ <- string "-to-"
              to <- munch1 isAlpha
              _ <- string " map:"
              return (from, to)

-- mapping impl

type Mapping = [(Int, Int, Int)]

-- useless type only for my understanding
type Sorted a = a

makeMapping :: [(Int, Int, Int)] -> Sorted Mapping
makeMapping = sortOn snd'
  where snd' (_, x, _) = x

-- for (60 56 37):xs
-- 1-55: 1-55
-- 56-92: 60-96
-- >92: use xs
get :: Int -> Sorted Mapping -> Int
get n [] = n
get n ((dest, src, range):ms)
  | n < src = get n ms
  | n < src + range = dest + n - src
  | otherwise = get n ms

-- part 2
-- life's bit harder here, now we'll do the complete opposite
-- to get the seed from the location

data Almanac' = Almanac' { seedsR                 :: [Int]
                         , seedToSoil'            :: Sorted Mapping'
                         , soilToFertilizer'      :: Sorted Mapping'
                         , fertilizerToWater'     :: Sorted Mapping'
                         , waterToLight'          :: Sorted Mapping'
                         , lightToTemperature'    :: Sorted Mapping'
                         , temperatureToHumidity' :: Sorted Mapping'
                         , humidityToLocation'    :: Sorted Mapping'
                         } deriving Show

parseAlmanac' :: String -> Maybe Almanac'
parseAlmanac' = fmap fst . find (null . snd) . readP_to_S almanac'

almanac' :: ReadP Almanac'
almanac' = do s <- seedsP
              blankLine
              [ ("seed", "soil", st)
                , ("soil", "fertilizer", sf)
                , ("fertilizer", "water", fw)
                , ("water", "light", wl)
                , ("light", "temperature", lt)
                , ("temperature", "humidity", th)
                , ("humidity", "location", hl)
                ] <- sepBy1 mapping' blankLine
              newline
              eof
              return $ Almanac' s st sf fw wl lt th hl

mapping' :: ReadP (String, String, Mapping')
mapping' = do (from, to) <- relation
              newline
              ms <- maps'
              return (from, to, ms)

maps' :: ReadP Mapping'
maps' = do triplets <- sepBy1 tripleInt newline
           return $ makeMapping' triplets

type Mapping' = Mapping

-- focus on dest this time
makeMapping' :: [(Int, Int, Int)] -> Sorted Mapping'
makeMapping' = sortOn fst'
  where fst' (x, _, _) = x

inSeedRange :: [Int]  -- seeds as in Almanac
            -> Int
            -> Bool
inSeedRange [] _ = False
inSeedRange (x:y:xs) n
  | n < x = inSeedRange xs n
  | n < x + y = True
  | otherwise = inSeedRange xs n

locToSeed :: Almanac'
          -> Int  -- location
          -> Int  -- seed
locToSeed Almanac'{..} loc = let humidity = reverseLookup loc humidityToLocation'
                                 temperature = reverseLookup humidity temperatureToHumidity'
                                 light = reverseLookup temperature lightToTemperature'
                                 water = reverseLookup light waterToLight'
                                 fertilizer = reverseLookup water fertilizerToWater'
                                 soil = reverseLookup fertilizer soilToFertilizer'
                              in reverseLookup soil seedToSoil'

reverseLookup :: Int
              -> Sorted Mapping'
              -> Int  -- bijection
reverseLookup n [] = n
reverseLookup n ((dest, src, range):ms)
  | n < dest = n
  | n < dest + range = src + n - dest
  | otherwise = reverseLookup n ms

main :: IO ()
main = do input <- readFile "input.txt"
          let Just a' = parseAlmanac' input
          let Just a = parseAlmanac input
          let seeds = map (locToSeed a') [0..]
          let seed = head $ filter (inSeedRange (seedsR a')) seeds
          print $ locationBySeed a seed

