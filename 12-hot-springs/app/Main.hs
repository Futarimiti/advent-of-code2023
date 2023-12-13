{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let pairs = map parseLine lined
          let arrangements's = map (uncurry arrangements) pairs
          let lengths = map length arrangements's
          print $ sum lengths

parseLine :: String -> ([Char], [Int])
parseLine s = (pat, read $ '[' : xs ++ "]")
                 where (pat, ' ':xs) = span (/= ' ') s

-- find possible arrangements that match the pattern and spec
arrangements :: [Char]  -- .??..??...?##.
             -> [Int]   -- 1,1,3
             -> [[Char]]  -- [".#...#....###", ".#....#...###", "..#..#....###", "..#...#...###"]
arrangements [] [] = [[]]
arrangements [] _ = []
arrangements cs [] | '#' `notElem` cs = [map (const '.') cs]
                   | otherwise = []
arrangements ('.':cs) spec = ('.':) <$> arrangements cs spec
arrangements "#" [1] = ["#"]
arrangements "#" _ = []
arrangements ('#':'.':cs) (1:spec) = ('#':) . ('.':) <$> arrangements cs spec
arrangements ('#':'.':_) _ = []
arrangements ('#':'#':cs) (n:spec) = ('#':) <$> arrangements ('#':cs) (n - 1:spec)
arrangements ('#':'?':cs) (1:spec) = ('#':) . ('.':) <$> arrangements cs spec
arrangements ('#':'?':cs) (n:spec) = ('#':) <$> arrangements ('#':cs) (n - 1:spec)
arrangements "?" [1] = ["#"]
arrangements "?" _ = []  -- _ not empty
arrangements ('?':c:cs) spec = is ++ isnext
  where is = arrangements ('#':c:cs) spec
        isnext = arrangements ('.':c:cs) spec
arrangements p _ = error $ "arrangements: unexpected pattern: " ++ p
