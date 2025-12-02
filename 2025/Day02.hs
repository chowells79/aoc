#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List

import Data.List.Split (chunksOf)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/02.txt"
         | otherwise = "example/02-" ++ show n ++ ".txt"

parse :: String -> [(Int, Int)]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy range comma <* skipSpaces <* eof

    comma = char ',' <* skipSpaces
    range = (,) <$> num <* char '-' <*> num

    num = read <$> munch1 isDigit

solve :: (Int -> Bool) -> [(Int, Int)] -> Int
solve invalid = sum . filter invalid . concatMap (uncurry enumFromTo)

part1 :: Int -> Bool
part1 x = firstHalf == secondHalf
  where
    s = show x
    (firstHalf, secondHalf) = splitAt (length s `div` 2) s

part2 :: Int -> Bool
part2 x = or $ map repeats [1 .. len `div` 2]
  where
    s = show x
    len = length s
    repeats n = case chunksOf n s of
                  (x:xs) -> all (== x) xs
                  _ -> False

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve part1 inp
    print $ solve part2 inp

