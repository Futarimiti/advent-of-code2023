-- https://adventofcode.com/2023/day/1#part1

module Main (main) where

import           Data.Char
import           Data.List

getCalibrationVal :: String -> Integer
getCalibrationVal = do d1 <- getFirstDigit
                       d2 <- getLastDigit
                       return $ read [d1, d2]

getLastDigit :: String -> Char
getLastDigit = getFirstDigit . reverse

getFirstDigit :: String -> Char
getFirstDigit (c:cs)
  | isDigit c = c
  | otherwise = getFirstDigit cs
getFirstDigit _ = error "no digit found"

main' :: IO ()
main' = do input <- readFile "input.txt"
           let lined = lines input
           let calibrated = map getCalibrationVal lined
           print $ sum calibrated

-- part two

-- replace all numbers in text with numeral
replaceNums :: String -> String
replaceNums "" = ""
replaceNums s@(c:cs) = case replaceNum s of
                  Just n  -> n : replaceNums cs
                  Nothing -> c : replaceNums cs

replaceNum :: String -> Maybe Char
replaceNum s
  | "one" `isPrefixOf` s = Just '1'
  | "two" `isPrefixOf` s = Just '2'
  | "three" `isPrefixOf` s = Just '3'
  | "four" `isPrefixOf` s = Just '4'
  | "five" `isPrefixOf` s = Just '5'
  | "six" `isPrefixOf` s = Just '6'
  | "seven" `isPrefixOf` s = Just '7'
  | "eight" `isPrefixOf` s = Just '8'
  | "nine" `isPrefixOf` s = Just '9'
  | otherwise = Nothing

getCalibrationVal' :: String -> Integer
getCalibrationVal' = getCalibrationVal . replaceNums


main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let calibrated = map getCalibrationVal' lined
          print $ sum calibrated
