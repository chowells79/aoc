#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Bits (xor, shiftR, shiftL)

import Data.List
import Data.Maybe

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

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


monkeyTriggers :: Int -> IntMap Int
monkeyTriggers x = addDeltas M.empty prices
  where
    prices = map (`rem` 10) . take 2001 $ iterate' step x
    addDeltas m (a:xs@(b:c:d:e:_)) = m' `seq` addDeltas m' xs
      where
        k = (10 + b - a) +
            (10 + c - b) * 100 +
            (10 + d - c) * 10000 +
            (10 + e - d) * 1000000
        m' = M.insertWith (const id) k e m
    addDeltas m _ = m


solve2 :: [Int] -> Int
solve2 monkeys = maximum bananas
  where
    triggers = map monkeyTriggers monkeys
    bananas = M.unionsWith (+) triggers


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
