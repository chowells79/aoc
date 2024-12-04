#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "04"


windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((== n) . length) . map (take n) . tails

substrings2dN :: Int -> String -> [String]
substrings2dN n s = concatMap (windows n) $ forwards ++ map reverse forwards
  where
    forwards = rows ++ cols ++ diag1s ++ diag7s
    rows = lines s
    cols = transpose rows
    justGrid = map (map Just) rows
    padNothings = zipWith id (iterate ((Nothing :) .) id)
    diagonalize = map catMaybes . transpose . padNothings
    diag1s = diagonalize justGrid
    diag7s = diagonalize $ reverse justGrid

solve1 :: String -> Int
solve1 = length . filter (== "XMAS") . substrings2dN 4


data Grid = G Int Int (Map (Int, Int) Char)
    deriving (Eq, Ord, Show)

parse2 :: String -> Grid
parse2 s = G r c b
  where
    b = M.fromList [ ((row, col), c)
                   | (row, line) <- zip [0..] (lines s)
                   , (col, c) <- zip [0..] line
                   ]
    r = length (lines s)
    c = length (head (lines s))

solve2 :: Grid -> Int
solve2 (G rowMax colMax board) =
    sum [ 1
        | ((r, c), 'A') <- M.toList board
        , r > 0, r < rowMax - 1, c > 0, c < colMax - 1
        , sort [board M.! (r - 1, c - 1), board M.! (r + 1, c + 1)] == "MS"
        , sort [board M.! (r - 1, c + 1), board M.! (r + 1, c - 1)] == "MS"
        ]


main :: IO ()
main = do
    inp <- input 0
    print $ solve1 inp

    print . solve2 . parse2 $ inp
