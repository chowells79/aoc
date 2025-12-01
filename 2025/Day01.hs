#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Applicative
import Data.List

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/01.txt"
         | otherwise = "example/01-" ++ show n ++ ".txt"

parse :: String -> [Int]
parse s = case readP_to_S full s of
            [(x, "")] -> scanl' (+) 50 x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = some line <* eof

    dir = id <$ char 'L' <|> negate <$ char 'R'
    num = read <$> munch1 isDigit
    line = dir <*> num <* skipSpaces


solve1 :: [Int] -> Int
solve1 = length . filter (== 0) . map (`mod` 100)


expand :: [Int] -> [Int]
expand (x:xs@(y:_)) = run ++ expand xs
  where
    run | x < y = [x, x + 1 .. y - 1]
        | x > y = [x, x - 1 .. y + 1]
        | otherwise = []
expand z = z

solve2 :: [Int] -> Int
solve2 = solve1 . expand


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
