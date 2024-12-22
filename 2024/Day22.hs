#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Bits (xor, shiftR, shiftL)

import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "22"

parse :: String -> [Int]
parse = map read . words

step :: Int -> Int
step x = third
  where
    first = ((x `shiftL` 6) `xor` x) `rem` modulus
    second = ((first `shiftR` 5) `xor` first) `rem` modulus
    third = ((second `shiftL` 11) `xor` second) `rem` modulus
    modulus = 16777216

solve1 :: [Int] -> Int
solve1 = sum . map ((!! 2000) . iterate' step)


monkeyTriggers :: Int -> Map (Int, Int, Int, Int) Int
monkeyTriggers x = addDeltas M.empty prices
  where
    prices = map (`rem` 10) . take 2001 $ iterate' step x
    addDeltas m (a:xs@(b:c:d:e:_))
        | M.member k m = addDeltas m xs
        | otherwise = m' `seq` addDeltas m' xs
      where
        k = (b - a, c - b, d - c, e - d)
        m' = M.insert k e m
    addDeltas m _ = m


solve2 :: [Int] -> Int
solve2 monkeys = maximum bananas
  where
    triggers = map monkeyTriggers monkeys
    bananas = do
        let range = [ -9 .. 9 ]
        k <- (,,,) <$> range <*> range <*> range <*> range
        pure $ sum [ fromMaybe 0 b | t <- triggers, let b = M.lookup k t ]


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
