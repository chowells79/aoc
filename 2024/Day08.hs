#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.List

import Data.Map (Map)
import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "08"


data AntMap = AM Int Int (Map Char [(Int, Int)])
    deriving (Eq, Ord, Show)

parse :: String -> AntMap
parse s = AM maxRow maxCol (M.fromListWith (++) antennae)
  where
    (_, [(maxRow, maxCol)]) = last coordinated
    antennae = filter (\(c, _) -> c /= '.') coordinated
    coordinated = [ (char, [(row, col)])
                  | (row, line) <- zip [0..] (lines s)
                  , (col, char) <- zip [0..] line
                  ]


antinodes1 :: AntMap -> [(Int, Int)]
antinodes1 (AM maxRow maxCol m) = sort
    [ candidate
    | (_, locs) <- M.toList m
    , ((rA, cA) : after) <- tails locs
    , (rB, cB) <- after
    , candidate <- let r = rA - rB ; c = cA - cB
                   in [(rA + r, cA + c), (rB - r, cB - c)]
    , inbounds candidate
    ]
  where
    inbounds (r, c) = r >= 0 && r <= maxRow && c >= 0 && c <= maxCol

solve1 :: AntMap -> Int
solve1 = length . nub . antinodes1


antinodes2 :: AntMap -> [(Int, Int)]
antinodes2 (AM maxRow maxCol m) = sort
    [ candidate
    | (_, locs) <- M.toList m
    , ((rA, cA) : after) <- tails locs
    , (rB, cB) <- after
    , candidate <- let r = rA - rB ; c = cA - cB
                   in ray (rA, cA) (r, c) ++ ray (rB, cB) (-r, -c)
    ]
  where
    ray (r0, c0) (dr, dc) = takeWhile inbounds
        [ (r0 + n * dr, c0 + n * dc) | n <- [0..] ]
    inbounds (r, c) = r >= 0 && r <= maxRow && c >= 0 && c <= maxCol

solve2 :: AntMap -> Int
solve2 = length . nub . antinodes2

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
