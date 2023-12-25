-- reddit.com/r/adventofcode/comments/18lwcw2/2023_day_19_an_equivalent_part_2_example_spoilers
-- thanks, Boojum!

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Part2 (main) where

import           Control.Applicative          (Alternative (some), (<|>))
import           Data.Char                    (isAlphaNum, isNumber)
import           Data.Map.Strict              (Map, (!))
import qualified Data.Map.Strict              as M
import           Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S,
                                               satisfy, string, (<++))

main :: IO ()
main = do input <- readFile "input.txt"
          let lined = lines input
          let workflowLines = takeWhile (/= "") lined
          let Just workflows = mapM parseNamedWorkflow workflowLines
          let flattened = concatMap (uncurry flatten) workflows
          let m = M.fromList flattened
          let tree = buildTree m
          let start = PartR (1, 4000) (1, 4000) (1, 4000) (1, 4000)
          print $ calc tree start

data Tree = TR
          | TA
          | Node { predicate :: Pred
                 , right     :: Tree
                 , left      :: Tree
                 } deriving (Eq, Show)

type Range a = (a, a)

data PartR = PartR { xr :: Range Integer
                   , mr :: Range Integer
                   , ar :: Range Integer
                   , sr :: Range Integer
                   } deriving (Eq, Show)

data Cat = X | M | A | S
  deriving (Eq, Show)

data Pred = Cat :< Integer
          | Cat :> Integer
          deriving (Eq, Show)

data Dest = Accepted | Rejected | Named String
  deriving (Eq, Show)

data Workflow = Tern Pred Workflow Workflow
              | Const Dest
              deriving (Eq, Show)

parseNamedWorkflow :: String -> Maybe (String, Workflow)
parseNamedWorkflow s = case readP_to_S namedWorkflow s of
                         [(w, "")] -> Just w
                         _         -> Nothing

namedWorkflow :: ReadP (String, Workflow)
namedWorkflow = do label <- munch1 (/= '{')
                   char '{'
                   wfs <- workflow
                   char '}'
                   return (label, wfs)
  where
    -- a<2006:qkq,m>2090:A,rfg
    workflow :: ReadP Workflow
    workflow = tern <|> const'

    const' :: ReadP Workflow
    const' = Const <$> dest

    dest :: ReadP Dest
    dest = accepted <++ rejected <++ named

    accepted = string "A" >> return Accepted
    rejected = string "R" >> return Rejected
    named = Named <$> munch1 isAlphaNum

    tern :: ReadP Workflow
    tern = do p <- predicate
              char ':'
              t <- const'
              char ','
              Tern p t <$> workflow

    predicate :: ReadP Pred
    predicate = lt <|> gt

    lt :: ReadP Pred
    lt = do c <- cat
            char '<'
            n <- int
            return $ c :< n

    gt :: ReadP Pred
    gt = do c <- cat
            char '>'
            n <- int
            return $ c :> n

    int :: ReadP Integer
    int = read <$> some (satisfy isNumber)

    cat :: ReadP Cat
    cat = x <|> m <|> a <|> s
    x = char 'x' >> return X
    m = char 'm' >> return M
    a = char 'a' >> return A
    s = char 's' >> return S

-- flatten a workflow from multiple tern cases to list of 1
-- px{a<2006:qkq,m>2090:A,rfg}
-- -> [(px, Tern a<2006 qkq pxF), (pxF, Tern m>2090 A rfg)]
flatten :: String -> Workflow -> [(String, Workflow)]
flatten label (Const d) = [(label, Const d)]
flatten label w@(Tern _ _ (Const _)) = [(label, w)]
flatten label (Tern p t (Tern p' t' f)) = (label, Tern p t nf) : flatten flabel (Tern p' t' f)
  where flabel = label ++ "F"
        nf = Const (Named flabel)

calc :: Tree -> PartR -> Integer
calc TR _ = 0
calc TA curr = productUp curr
  where productUp PartR{..} = product (diff <$> [xr, mr, ar, sr])
        diff (b, t) = 1 + fromIntegral (t - b)
calc Node{..} curr = calc right (updateBy predicate curr)
                   + calc left (updateAgainst predicate curr)
  where
    -- i apologise to lens
    updateBy :: Pred -> PartR -> PartR
    updateBy (c :< n) curr = case c of
                               X -> let (bot, top) = xr curr in curr { xr = (bot, min (n - 1) top) }
                               M -> let (bot, top) = mr curr in curr { mr = (bot, min (n - 1) top) }
                               A -> let (bot, top) = ar curr in curr { ar = (bot, min (n - 1) top) }
                               S -> let (bot, top) = sr curr in curr { sr = (bot, min (n - 1) top) }
    updateBy (c :> n) curr = case c of
                               X -> let (bot, top) = xr curr in curr { xr = (max (n + 1) bot, top) }
                               M -> let (bot, top) = mr curr in curr { mr = (max (n + 1) bot, top) }
                               A -> let (bot, top) = ar curr in curr { ar = (max (n + 1) bot, top) }
                               S -> let (bot, top) = sr curr in curr { sr = (max (n + 1) bot, top) }

    updateAgainst :: Pred -> PartR -> PartR
    updateAgainst (c :< n) curr = case c of
                                    X -> let (bot, top) = xr curr in curr { xr = (max n bot, top) }
                                    M -> let (bot, top) = mr curr in curr { mr = (max n bot, top) }
                                    A -> let (bot, top) = ar curr in curr { ar = (max n bot, top) }
                                    S -> let (bot, top) = sr curr in curr { sr = (max n bot, top) }
    updateAgainst (c :> n) curr = case c of
                                    X -> let (bot, top) = xr curr in curr { xr = (bot, min n top) }
                                    M -> let (bot, top) = mr curr in curr { mr = (bot, min n top) }
                                    A -> let (bot, top) = ar curr in curr { ar = (bot, min n top) }
                                    S -> let (bot, top) = sr curr in curr { sr = (bot, min n top) }

-- build a tree from a map of workflows, starting with "in"
buildTree :: Map String Workflow -> Tree
buildTree = buildNode "in"
  where
    buildNode :: String -> Map String Workflow -> Tree
    buildNode key m = let Tern p (Const t) (Const f) = m ! key
                          next = \case Accepted -> TA
                                       Rejected -> TR
                                       Named k  -> buildNode k m
                      in Node p (next t) (next f)

