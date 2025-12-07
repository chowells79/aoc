#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}
{-# Language BangPatterns #-}

import Data.List (mapAccumL)

import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.Map.Strict as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/07.txt"
         | otherwise = "example/07-" ++ show n ++ ".txt"

type Input = (Set Int, [Set Int])

parse :: String -> Input
parse s = (start, filter (not . S.null) splitters)
  where
    (first:rest) = lines s
    start = chars 'S' first
    splitters = map (chars '^') rest
    chars c r = S.fromList [ i | (x, i) <- zip r [0..], x == c ]


solve :: Input -> (Int, Int)
solve (starts, splitters) = ( sum $ map S.size hits
                            , sum $ M.restrictKeys routes starts
                            )
  where
    (final, hits) = mapAccumL collide starts splitters
    collide input splits = (next, hit)
      where
        !next = S.union continuing new
        hit = S.intersection input splits
        continuing = S.difference input hit
        new = S.fromList [ i | x <- S.toList hit, i <- [x - 1, x + 1] ]

    routes = foldl' addRoutes (M.fromSet (const 1) final) $ reverse hits
    addRoutes old hit = new `M.union` old
      where
        new = M.fromSet (\i -> old M.! (i - 1) + old M.! (i + 1)) hit


main :: IO ()
main = do
    (part1, part2) <- solve . parse <$> input 0
    print part1
    print part2
