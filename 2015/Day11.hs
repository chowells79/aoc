#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Data.Char (isAlpha)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/11.txt"
         | otherwise = "example/11-" ++ show n ++ ".txt"

parse :: String -> String
parse = filter isAlpha

nexts :: String -> [String]
nexts = drop 1 . go
  where
    go [] = [""]
    go (c:cs) = do
        c' <- filter (not . (`elem` "oil")) [c .. 'z']
        let cs' | c' == c = cs
                | otherwise = replicate (length cs) 'a'
        cs'' <- go cs'
        pure $ c' : cs''

solve1 :: String -> String
solve1 = head . filter valid . nexts
  where
    valid s = straight s && twoPair s

    straight [] = False
    straight (x:xs) = two x xs
      where
        two a [] = False
        two a (c:cs)
            | c == succ a = three a c cs
            | otherwise = two c cs

        three a b [] = False
        three a b (c:cs)
            | c == succ b = True
            | otherwise = two c cs

    twoPair = go 0
      where
        go 2 _ = True
        go i (c1:c2:cs)
            | c1 == c2 = go (i + 1) cs
            | otherwise = go i (c2:cs)
        go _ _ = False

solve2 :: String -> String
solve2 = solve1 . solve1

main :: IO ()
main = do
    inp <- parse <$> input 0
    putStrLn $ solve1 inp
    putStrLn $ solve2 inp
