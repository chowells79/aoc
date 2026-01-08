#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List (elemIndices, iterate')

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/18.txt"
         | otherwise = "example/18-" ++ show n ++ ".txt"

parse :: String -> Set (Int, Int)
parse s = S.fromList [ (r, c)
                     | (r, l) <- zip [0..] (lines s)
                     , c <- elemIndices '#' l
                     ]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors p@(r, c) = filter (/= p) [ (r', c')
                                   | r' <- [ r - 1, r, r + 1]
                                   , c' <- [ c - 1, c, c + 1 ]
                                   , r' >= 0 && r' < 100
                                   , c' >= 0 && c' < 100
                                   ]

step :: Set (Int, Int) -> Set (Int, Int)
step s = S.fromList [ p | (p, c) <- M.toList counts, alive p c ]
  where
    counts = M.fromListWith (+) [(p', 1) | p <- S.toList s, p' <- neighbors p]
    alive p c | p `S.member` s = c == 2 || c == 3
              | otherwise = c == 3

solve1 :: Set (Int, Int) -> Int
solve1 = S.size . (!! 100) . iterate' step


addCorners :: Set (Int, Int) -> Set (Int, Int)
addCorners = S.union $ S.fromList [ (0, 0), (0, 99), (99, 99), (99, 0) ]

solve2 :: Set (Int, Int) -> Int
solve2 = S.size . (!! 100) . iterate' (addCorners . step) . addCorners


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
