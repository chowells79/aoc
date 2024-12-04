#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "02"

parse :: String -> [[Int]]
parse = map (map read . words) . lines

safe :: [Int] -> Bool
safe xs = all inbounds diffs || all inbounds (map negate diffs)
  where
    diffs = zipWith (-) xs (tail xs)
    inbounds x = x >= 1 && x <= 3

solve1 :: [[Int]] -> Int
solve1 = length . filter safe

select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- select xs ]

safe2 :: [Int] -> Bool
safe2 xs = any safe candidates
  where
    candidates = xs : map snd (select xs)


solve2 :: [[Int]] -> Int
solve2 = length . filter safe2

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
