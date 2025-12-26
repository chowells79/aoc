#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Data.Char (isDigit)
import Data.List (group)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/10.txt"
         | otherwise = "example/10-" ++ show n ++ ".txt"

parse :: String -> [Int]
parse = map (read . (:[])) . filter isDigit

lookAndSay :: [Int] -> [Int]
lookAndSay = concatMap (\r -> [length r, head r]) . group

solve :: Int -> [Int] -> Int
solve c xs = length $ iterate lookAndSay xs !! c

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve 40 inp
    print $ solve 50 inp
