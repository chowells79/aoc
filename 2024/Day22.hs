#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.List
import Data.Bits

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/22.txt"
         | otherwise = "example/22-" ++ show n ++ ".txt"

step :: Int -> Int
step = roll 11 . roll (-5) . roll 6
  where
    roll d x = ((x `shift` d) `xor` x) .&. (1 `shiftL` 24 - 1)

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
    inp <- map read . words <$> input 0
    print $ solve1 inp
    print $ solve2 inp
