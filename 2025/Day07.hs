#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}
{-# Language BangPatterns #-}

import Data.Bitraversable (firstA)
import Data.List (elemIndices, mapAccumL)

import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.Map.Strict as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/07.txt"
         | otherwise = "example/07-" ++ show n ++ ".txt"

parse :: String -> (Set Int, [Set Int])
parse s = (starts, filter (not . S.null) splitters)
  where
    (first:rest) = lines s
    starts = locs 'S' first
    splitters = map (locs '^') rest
    locs c = S.fromList . elemIndices c


solve :: Set Int -> [Set Int] -> (Int, Int)
solve startSet splitterRows = (splittersUsed, routeCount)
  where
    !splittersUsed = sum $ map M.size hitCounts
    !routeCount = sum finalCounts

    (finalCounts, hitCounts) = mapAccumL step startCounts splitterRows
    step beams splitters = (beamsOut, hit)
      where
        (hit, passed) = M.partitionWithKey (\k _ -> S.member k splitters) beams
        new = M.fromListWith (+) $ firstA (\i -> [i-1, i+1]) =<< M.assocs hit
        !beamsOut = M.unionWith (+) new passed

    startCounts = M.fromSet (const 1) startSet


main :: IO ()
main = do
    (part1, part2) <- uncurry solve . parse <$> input 0
    print part1
    print part2
