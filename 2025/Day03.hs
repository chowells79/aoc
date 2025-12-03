#!/usr/bin/env cabal
{- cabal:
build-depends: base, utility-ht
-}

import Data.List.HT

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/03.txt"
         | otherwise = "example/03-" ++ show n ++ ".txt"

solve :: Int -> [String] -> Int
solve n = sum . map (read . maxSubsequence n)

-- Explodes if you call it with a count greater than the length of the
-- list. Don't do that.
maxSubsequence :: Ord a => Int -> [a] -> [a]
maxSubsequence c = go c . countDown
  where
    go 0 _ = []
    go n xs = y : go (n - 1) ys
      where
        ((y, _):ys) = maximum . takeUntil ((== n) . snd . head) . tails $ xs

countDown :: [a] -> [(a, Int)]
countDown xs = let len = length xs in zip xs [len, len - 1 ..]

main :: IO ()
main = do
    inp <- lines <$> input 0
    print $ solve 2 inp
    print $ solve 12 inp
