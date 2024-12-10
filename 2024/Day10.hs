#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "10"

parse :: String -> Map (Int, Int) Int
parse s = M.fromList
    [ ((row, col), read (pure char))
    | (row, line) <- zip [0..] $ lines s
    , (col, char) <- zip [0..] $ line
    ]

flood09 :: Map (Int, Int) Int -> (Int, Int) -> Set (Int, Int)
flood09 b = go 0 . S.singleton
  where
    -- go n s | traceShow (n, s) False = undefined
    go 9 s = s
    go n s = go n' s'
      where
        n' = n + 1
        s' = S.fromList [ c | p <- toList s, c <- neighbors p
                            , M.lookup c b == Just n' ]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]

trailheads :: Map (Int, Int) Int -> [Set (Int, Int)]
trailheads b = [ flood09 b c | (c, 0) <- M.toList b ]

trailheadScores :: Map (Int, Int) Int -> [Int]
trailheadScores = map S.size . trailheads

solve1 :: Map (Int, Int) Int -> Int
solve1 = sum . trailheadScores


count09 :: Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
count09 b = go 0 . pure
  where
    go 9 l = l
    go n l = go n' l'
      where
        n' = n + 1
        l' = [ c | p <- l, c <- neighbors p , M.lookup c b == Just n' ]

trailheadRatings :: Map (Int, Int) Int -> [Int]
trailheadRatings b = map length [ count09 b c | (c, 0) <- M.toList b ]

solve2 :: Map (Int, Int) Int -> Int
solve2 = sum . trailheadRatings

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
