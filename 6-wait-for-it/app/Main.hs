{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

-- main :: IO ()
-- main = do input <- readFile "input.txt"
--           let [time, distance] = lines input
--           let "Time:" : times = words time
--           let "Distance:" : distances = words distance
--           let pairs = zip (read <$> times) (read <$> distances)
--           let margins = map (uncurry margin) pairs
--           print $ product margins

-- how much margin of error you have
-- by determining number of ways the record could be broken
margin :: Int  -- time allowed
       -> Int  -- record distance
       -> Int
margin time target = length $ filter (> target) $ map (uncurry travelled) holdNtravel
  where holdNtravel = zip [0..time] [time, time-1..]

travelled :: Int  -- time for holding the button
          -> Int  -- travel time
          -> Int  -- distance
travelled hold travel = speed * travel
  where speed = hold

-- part 2

-- nahh thanks ill just brute force it
main :: IO ()
main = do input <- readFile "input.txt"
          let [timeStr, distanceStr] = lines input
          let "Time:" : timesStr = words timeStr
          let "Distance:" : distancesStr = words distanceStr
          let time = read $ concat timesStr
          let distance = read $ concat distancesStr
          let m = margin time distance
          print m

