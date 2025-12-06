#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
-}
{-# Language LambdaCase #-}

import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Split (wordsBy)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/06.txt"
         | otherwise = "example/06-" ++ show n ++ ".txt"

parse1 :: String -> [(String, [Int])]
parse1 s = zip ops nums
  where
    (ops:numRows) = map words . reverse . lines $ s
    nums = map (map read) . transpose $ reverse numRows

parse2 :: String -> [(String, [Int])]
parse2 s = zip (words opRow) nums
  where
    (opRow:numRows) = reverse $ lines s
    nums = map (map read) . groupNums . transpose $ reverse numRows
    groupNums = wordsBy null . map (filter isDigit)

solve :: [(String, [Int])] -> Int
solve = sum . map (uncurry $ \case "*" -> product ; "+" -> sum)

main :: IO ()
main = do
    inp <- input 0
    print . solve . parse1 $ inp
    print . solve . parse2 $ inp
