#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, array
-}
{-# Language BangPatterns, LambdaCase #-}

import Data.List (mapAccumL)

import Data.Set (Set)
import qualified Data.Set as S

import Data.Array.Unboxed (UArray, (!), genArray)

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


solve1 :: Input -> Int
solve1 = sum . map S.size . snd . uncurry (mapAccumL step)
  where
    step input splitters = (output, hits)
      where
        !output = S.union continuing new
        hits = S.intersection input splitters
        continuing = S.difference input hits
        new = S.fromList [ i | x <- S.toList hits, i <- [x - 1, x + 1] ]


solve2 :: Input -> Int
solve2 (start, splitters) = sum [ result ! s | s <- S.toList start ]
  where
    b = (-1, maximum (S.unions splitters) + 1)
    initial = genArray b (const 1) :: UArray Int Int
    result = foldl' step initial $ reverse splitters
    step prev splits = genArray b $
        \case i | i `S.member` splits -> prev ! (i - 1) + prev ! (i + 1)
                | otherwise -> prev ! i


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
