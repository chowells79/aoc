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

-- returns a list of all the multi-character strings in the data
parse1 :: String -> [[Char]]
parse1 s = filter ((> 1) . length) . concatMap inits . concatMap tails $
    forwards ++ map reverse forwards
  where
    forwards = rows ++ cols ++ diag1s ++ diag7s
    rows = lines s
    cols = transpose rows
    diag1s = map catMaybes . transpose $ zipWith (++) (iterate (Nothing :) []) (map (map Just) rows)
    diag7s = map catMaybes . transpose $ zipWith (++) (iterate (Nothing :) []) (reverse $ map (map Just) rows)


solve1 :: String -> String -> Int
solve1 inp target = length . filter (== target) . parse1 $ inp


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
    print $ solve1 inp "XMAS"

    print . solve2 . parse2 $ inp
