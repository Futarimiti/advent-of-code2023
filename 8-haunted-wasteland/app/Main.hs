{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import           Data.Map (Map, fromList, keys, (!))

data Node = Node { left :: String, right :: String }
  deriving (Show)

type Name = String

readNode :: String -> (Name, Node)
readNode line = let (name, drop 4 -> rest) = splitAt 3 line
                    (l, drop 2 -> rest2) = splitAt 3 rest
                    r = take 3 rest2
                 in (name, Node l r)

readNodes :: [String] -> Map Name Node
readNodes = fromList . map readNode

type Directions = [Char]
steps :: Directions
      -> Name  -- start
      -> Map Name Node
      -> Int
steps = steps' 0
  where
    steps' :: Int -> Directions -> Name -> Map Name Node -> Int
    steps' acc _ "ZZZ" _ = acc
    steps' acc ('L':ds) curr nodes = let node = nodes ! curr
                                         next = left node
                                      in steps' (acc + 1) ds next nodes
    steps' acc ('R':ds) curr nodes = let node = nodes ! curr
                                         next = right node
                                      in steps' (acc + 1) ds next nodes

-- main :: IO ()
-- main = do input <- readFile "input.txt"
--           let directionsStr : _ : nodesStr = lines input
--           let nodes = readNodes nodesStr
--           let directions = cycle directionsStr
--           let result = steps directions "AAA" nodes
--           print result

-- part 2

-- brute force won't do

steps' :: Directions
       -> Map Name Node
       -> Name           -- start
       -> Int            -- steps taken
steps' = steps'' 0
  where steps'' :: Int -> Directions -> Map Name Node -> Name -> Int
        steps'' acc _ _ (last -> 'Z') = acc
        steps'' acc ('L':ds) nodes curr = let node = nodes ! curr
                                              next = left node
                                           in steps'' (acc + 1) ds nodes next
        steps'' acc ('R':ds) nodes curr = let node = nodes ! curr
                                              next = right node
                                           in steps'' (acc + 1) ds nodes next

main :: IO ()
main = do input <- readFile "input.txt"
          (directionsStr : _ : nodesStr) <- return $ lines input
          let nodes = readNodes nodesStr
          let directions = cycle directionsStr
          let starts = filter ((== 'A') . last) $ keys nodes
          let steps = map (steps' directions nodes) starts
          let result = foldr1 lcm steps  -- who would have thought?
          print result
