#!/usr/bin/env cabal
{- cabal:
build-depends: base, array
-}
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List (sort, tails)

import qualified Data.Array as A

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/17.txt"
         | otherwise = "example/17-" ++ show n ++ ".txt"

parse :: String -> [Int]
parse = map read . lines

solve1 :: Int -> [Int] -> Int
solve1 target caps' = results A.! (1, target)
  where
    cBounds = (1, length caps')
    caps = A.listArray cBounds caps'

    rBounds = ((fst cBounds, 0), (snd cBounds, target))
    results = A.listArray rBounds [ go i j | (i, j) <- A.range rBounds ]

    go i j | j == 0 = 1
           | i == snd cBounds = if j == caps A.! i then 1 else 0
           | otherwise = sum [ results A.! ((i + 1), j')
                             | j' <- filter (>= 0) [ j, j - (caps A.! i) ]
                             ]


nOf :: Int -> [a] -> [[a]]
nOf 1 xs = map pure xs
nOf n xs | n < 1 = []
         | otherwise = [ y:zs | (y:ys) <- tails xs, zs <- nOf (n - 1) ys ]

solve2 :: Int -> [Int] -> Int
solve2 target xs = length $ head
    [ hits
    | n <- [ 0 .. ]
    , let { sums = map sum $ nOf n xs
          ; hits = filter (== target) sums
          }
    , not $ null hits
    ]


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 150 inp
    print $ solve2 150 inp
