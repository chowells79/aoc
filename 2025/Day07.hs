#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}
{-# Language BangPatterns #-}

import Data.List (elemIndices, mapAccumL)

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
parse s = (starts, filter (not . S.null) splitters)
  where
    (first:rest) = lines s
    starts = locs 'S' first
    splitters = map (locs '^') rest
    locs c = S.fromList . elemIndices c


solve :: Input -> (Int, Int)
solve (starts, splitters) = (hitCount, routeCount)
  where
    !hitCount = sum $ map S.size hits
    (final, hits) = mapAccumL collide starts splitters
    collide input splits = (next, hit)
      where
        !next = S.union continuing new
        hit = S.intersection input splits
        continuing = S.difference input hit
        new = S.fromList [ i | x <- S.toList hit, i <- [x - 1, x + 1] ]

    !routeCount = sum $ M.restrictKeys routes starts
    routes = foldl' addRoutes (M.fromSet (const 1) final) $ reverse hits
    addRoutes old hit = M.union new old
      where
        new = M.fromSet (\i -> old M.! (i - 1) + old M.! (i + 1)) hit


main :: IO ()
main = do
    (part1, part2) <- solve . parse <$> input 0
    print part1
    print part2
