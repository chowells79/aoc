#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}
{-# Language BangPatterns #-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/05.txt"
         | otherwise = "example/05-" ++ show n ++ ".txt"

type Input = ([(Int, Int)], [Int])

parse :: String -> Input
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = (,) <$> endBy range nl <* nl <*> endBy num nl <* eof
    nl = char '\n'
    range = (,) <$> num <* char '-' <*> num
    num = read <$> munch1 isDigit

solve1 :: Input -> Int
solve1 (ranges, ids) = length (filter inRange ids)
  where
    inRange x = or [ l <= x && x <= h | (l, h) <- ranges ]

solve2 :: Input -> Int
solve2 (ranges, _) = go 0 0 (sort ranges)
  where
    go !x _ [] = x
    go !x i ((nextL, nextH):rest)
        | nextH < i = go x i rest
        | otherwise = go (x + nextH - max i nextL + 1) (nextH + 1) rest

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
