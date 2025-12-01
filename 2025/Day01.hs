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
    name | n == 0 = "input/01.txt"
         | otherwise = "example/01-" ++ show n ++ ".txt"

parse :: String -> [Int]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = many line <* eof

    dir = (id <$ char 'L') +++ (negate <$ char 'R')
    num = read <$> munch1 isDigit
    line = dir <*> num <* char '\n'


start, size :: Int
start = 50
size = 100

solve1 :: [Int] -> Int
solve1 = length . filter (== 0) . map (`mod` size)

solve2 :: [Int] -> Int
solve2 xs = foldl' (+) 0 $ zipWith zeroes xs (drop 1 xs)

zeroes :: Int -> Int -> Int
zeroes from to
    | from > to = zeroes to from - atZero from + atZero to
    | otherwise = (to - from) `div` size + extra
  where
    atZero x = if x `mod` size == 0 then 1 else 0
    extra = if (to `mod` size) < (from `mod` size) then 1 else 0


main :: IO ()
main = do
    points <- scanl' (+) start . parse <$> input 0
    print $ solve1 points
    print $ solve2 points
