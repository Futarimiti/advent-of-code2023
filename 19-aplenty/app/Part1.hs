{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Part1 (main) where

import           Data.Char           (isNumber)
import           Data.List.Split     (splitOn)
import           Data.Map            (Map, (!))
import qualified Data.Map            as M
import           Text.Show.Functions ()

type Cat = Char

data Pred = Lt Cat Int
          | Gt Cat Int
          deriving (Show, Eq)

-- categories of a part
data Part = Part { x :: Int
                 , m :: Int
                 , a :: Int
                 , s :: Int
                 } deriving (Eq, Show, Read)

parseCat :: Char -> Part -> Int
parseCat = \case 'x' -> x
                 'm' -> m
                 'a' -> a
                 's' -> s
                 _   -> error "wtf?"

data Dest = A | R | Named String
  deriving (Eq, Show)

data Rule = Const Dest
          | If Pred Dest
          deriving (Show)

type Workflows = Map String (Part -> Dest)

parseParts :: [String] -> [Part]
parseParts = map parsePart
  where
    parsePart :: String -> Part
    parsePart = read . ("Part " ++)

parseWorkflows :: [String] -> Workflows
parseWorkflows = M.fromList . map parseWorkflow
  where
    parseDest :: String -> Dest
    parseDest "A"  = A
    parseDest "R"  = R
    parseDest name = Named name

    parseRule :: String -> Rule
    parseRule (cat:'<':rest) = let (val, _:next) = span isNumber rest
                                in If (Lt cat (read val)) (parseDest next)
    parseRule (cat:'>':rest) = let (val, _:next) = span isNumber rest
                                in If (Gt cat (read val)) (parseDest next)
    parseRule str = Const $ parseDest str

    -- "a<2006:qkq,m>2090:A,rfg" -> \p -> if a p < 2006 then qkq else if m p > 2090 then A else rfg
    parseRules :: String -> Part -> Dest
    parseRules = foldRules . map parseRule . splitOn ","
      where foldRules :: [Rule] -> Part -> Dest
            foldRules [Const dest] _ = dest
            foldRules ((If (Lt ch val) dest):rules) p
              | cat p < val = dest
              | otherwise = foldRules rules p
              where cat = parseCat ch
            foldRules ((If (Gt ch val) dest):rules) p
              | cat p > val = dest
              | otherwise = foldRules rules p
              where cat = parseCat ch
            foldRules _ _ = error "wtf?"

    parseWorkflow :: String -> (String, Part -> Dest)
    parseWorkflow line = (name, rule)
      where (name, _:ruleStr) = break (== '{') line
            rule = parseRules $ init ruleStr

isAcceptedBy :: Part -> Workflows -> Bool
part `isAcceptedBy` workflows = isAcceptedBy1 "in" part workflows
  where
    isAcceptedBy1 :: String -> Part -> Workflows -> Bool
    isAcceptedBy1 curr p wf = let start = wf ! curr
                               in case start p of
                                    R          -> False
                                    A          -> True
                                    Named name -> isAcceptedBy1 name p wf

sumRatings :: Part -> Int
sumRatings Part{..} = x + m + a + s

main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let (workflowsLines, _:partsLines) = break (== "") lined
          let workflows = parseWorkflows workflowsLines
          let parts = parseParts partsLines
          let goodParts = filter (`isAcceptedBy` workflows) parts
          let ratings = sumRatings <$> goodParts
          print $ sum ratings
