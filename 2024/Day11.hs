#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.List (iterate')
import Data.Map (Map, fromListWith, toList)

input :: Integer -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "11"

parse :: String -> Map Integer Integer
parse = fromListWith (+) . flip zip (repeat 1) . map read . words

step :: Integer -> [Integer]
step n
    | n == 0 = [1]
    | parity == 0 = map read [upper, lower]
    | otherwise = [n * 2024]
  where
    strN = show n
    (half, parity) = length strN `quotRem` 2
    (upper, lower) = splitAt half strN

blink :: Map Integer Integer -> Map Integer Integer
blink prev = fromListWith (+)
    [ (n, count)
    | (i, count) <- toList prev
    , n <- step i
    ]

multiblink :: Int -> Map Integer Integer -> Map Integer Integer
multiblink n = head . drop n . iterate' blink

solve :: Int -> Map Integer Integer -> Integer
solve n = sum . multiblink n

main :: IO ()
main = do
    inp <- parse <$> input 0

    print $ solve 25 inp
    print $ solve 75 inp
