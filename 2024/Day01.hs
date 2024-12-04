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
    ident = "01"

parse :: String -> [[Int]]
parse = map (map read . words) . lines

sortTransposed :: Ord a => [[a]] -> [[a]]
sortTransposed = transpose . map sort . transpose

dist :: [Int] -> Int
dist = foldl' (\acc x -> abs (acc - x)) 0

counts :: Ord a => [a] -> Map a Int
counts = M.fromListWith (+) . flip zip (repeat 1)

main :: IO ()
main = do
    s <- input 0
    let parsed = parse s
        part1 = sum . map dist . sortTransposed $ parsed
    print part1

    let [left, right] = transpose parsed
        rightCounts = counts right
        score = zipWith (*) <*> map (\x -> M.findWithDefault 0 x rightCounts)
        part2 = sum . score $ left
    print part2
