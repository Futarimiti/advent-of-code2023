{-# LANGUAGE ViewPatterns #-}

module Main (main', main) where

import           Control.Applicative ((<|>))
import           Control.Monad       (mfilter)
import           Data.Char           (isDigit, ord)
import           Data.DList          (DList)
import qualified Data.DList          as D
import           Data.IntMap.Strict  (IntMap, adjust, alter, mapWithKey)
import           Data.List           (dropWhileEnd, foldl')
import           Data.List.Split     (splitOn)
import           Data.Maybe          (mapMaybe)
import           Text.Read           (readMaybe)

ascii :: Char -> Int
ascii = ord

hash :: String -> Int
hash = foldl' (\acc c -> (acc + ascii c) * 17 `mod` 256) 0

parseSeqs :: String -> [String]
parseSeqs = splitOn ","

main' :: IO ()
main' = do raw <- readFile "input.txt"
           let input = dropWhileEnd (== '\n') raw
           let seqs = parseSeqs input
           let vals = map hash seqs
           print $ sum vals

-- part 2

type Label = String
type Focus = Int
data Lens = Lens { name  :: Label
                 , focus :: Focus
                 } deriving (Show, Eq)
type Boxes = IntMap (DList Lens)

corresBox :: Label -> Int
corresBox = hash

data Operation = Update Label Focus
               | Remove Label
               deriving (Show, Eq)

perform :: Operation -> Boxes -> Boxes
perform (Update label focus) = alter update (corresBox label)
  where lens = Lens label focus
        update :: Maybe (DList Lens) -> Maybe (DList Lens)
        update Nothing = Just $ D.singleton lens
        update (Just old)
          | null old = Just $ D.singleton lens
          | any ((== label) . name) old = Just $ (\l -> if name l == label then l{focus=focus} else l) <$> old
          | otherwise = Just $ D.snoc old lens
perform (Remove label)       = adjust remove (corresBox label)
  where remove = mfilter ((label /=) . name)

parseStep :: String -> Maybe Operation
parseStep s = update <|> remove
  where remove = Just . Remove . takeWhile (/= '-') $ s
        update = do f <- readMaybe focus
                    return $ Update label f
                      where label = takeWhile (/= '=') s
                            focus = dropWhile (not . isDigit) s


parseSteps :: String -> [Operation]
parseSteps = mapMaybe parseStep . parseSeqs

boxPower :: Int         -- box number (0-based)
         -> DList Lens
         -> Int
boxPower ((+1) -> i) (D.toList -> lenses) =
  i * sum (zipWith (*) [1..] (map focus lenses))

power :: Boxes -> Int
power = sum . mapWithKey boxPower

main :: IO ()
main = do raw <- readFile "input.txt"
          let input = dropWhileEnd (== '\n') raw
          let ops = parseSteps input
          let performs = perform <$> ops
          let composed = foldr1 (flip (.)) performs
          let final = composed mempty
          print $ power final

