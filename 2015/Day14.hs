#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit)

import Data.List (scanl', transpose)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/14.txt"
         | otherwise = "example/14-" ++ show n ++ ".txt"

parse :: String -> [(String, Int, Int, Int)]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy line (char '\n') <* skipSpaces <* eof
    line = (,,,) <$> name <* string " can fly " <*> num <* string " km/s for " <*> num <* string " seconds, but then must rest for " <*> num <* string " seconds."
    name = munch1 isAlpha
    num = read <$> munch1 isDigit

simulate :: Int -> Int -> Int -> [Int]
simulate v d r = scanl' (+) 0 $ cycle rep
  where
    rep = replicate d v ++ replicate r 0

solve1 :: [(String, Int, Int, Int)] -> Int
solve1 reindeer = maximum (at 2503)
  where
    at i = map (!! i) distances
    distances = [ simulate v d r | (_, v, d, r) <- reindeer ]

solve2 :: [(String, Int, Int, Int)] -> Int
solve2 reindeer = maximum (scores !! 2503)
  where
    scores = scanl' (\p c -> zipWith (+) p $ leaders c) (repeat 0) distances
    leaders xs = let m = maximum xs in map (\c -> if c == m then 1 else 0) xs
    distances = drop 1 $ transpose [ simulate v d r | (_, v, d, r) <- reindeer ]

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
