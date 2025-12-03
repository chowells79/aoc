#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Data.List

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/03.txt"
         | otherwise = "example/03-" ++ show n ++ ".txt"

parse :: String -> [[Int]]
parse s = map (read . pure) <$> lines s

solve1 :: [[Int]] -> Int
solve1 = sum . map maxJolt
  where
    maxJolt xs = maximum [ 10 * a + b | (a:as) <- tails xs, b <- as ]

solve2 :: [[Int]] -> Int
solve2 = sum . map maxUnsafeJolt

maxUnsafeJolt :: [Int] -> Int
maxUnsafeJolt = uncurry go . splitAt 12
  where
    go current [] = read . concat . map show $ current
    go current (x:xs) = go (maximum candidates) xs
      where
        candidates = dels (current ++ [x])

dels :: [a] -> [[a]]
dels [] = []
dels (x:xs) = xs : map (x:) (dels xs)

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
