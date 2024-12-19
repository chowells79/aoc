#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}
{-# Language BangPatterns #-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List

import Data.Map (Map)
import qualified Data.Map.Lazy as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "19"

parse :: String -> ([Towel], [Pattern])
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = (,) <$> towels <* char '\n' <*> patterns <* eof

    towels = sepBy1 towel (string ", ") <* char '\n'
    towel = many1 color

    patterns = endBy1 pat (char '\n')
    pat = many1 color

    color = choice
        [ W <$ char 'w'
        , U <$ char 'u'
        , B <$ char 'b'
        , R <$ char 'r'
        , G <$ char 'g'
        ]


data Color = W | U | B | R | G deriving (Eq, Ord, Show)

type Towel = [Color]
type Pattern = [Color]


arrangements :: [Towel] -> Pattern -> Int
arrangements towels pat = results M.! pat
  where
    results = M.fromList [ (p, arrs p)
                         | p <- tails pat
                         ]
    arrs :: Pattern -> Int
    arrs p = sum $ do
        t <- towels
        case stripPrefix t p of
            Nothing -> []
            Just [] -> [1]
            Just xs -> [results M.! xs]


solve1 :: [Towel] -> [Pattern] -> Int
solve1 towels = length . filter ((/= 0) . arrangements towels)

solve2 :: [Towel] -> [Pattern] -> Int
solve2 towels = sum . map (arrangements towels)

main :: IO ()
main = do
    (ts, ps) <- parse <$> input 0
    print $ solve1 ts ps
    print $ solve2 ts ps
