module Main (main', main) where

parseSeq :: String -> [Int]
parseSeq = fmap read . words

-- [0,3,6,9,12,15] -> 18
-- [1,3,6,10,15,21] -> 28
next :: [Int] -> Int
next xs
  | all (== 0) diffs = head xs
  | otherwise = last xs + next diffs
  where diffs = zipWith (-) (tail xs) xs

main' :: IO ()
main' = do input <- readFile "input.txt"
           let seqs = parseSeq <$> lines input
           let res = sum $ next <$> seqs
           print res

-- ...seriously? you call this part 2?

main :: IO ()
main = do input <- readFile "input.txt"
          let seqs = parseSeq <$> lines input
          let res = sum $ next . reverse <$> seqs
          print res
