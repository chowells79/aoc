#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit)

import Data.List (nub, permutations)

import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/13.txt"
         | otherwise = "example/13-" ++ show n ++ ".txt"

parse :: String -> [(String, Int, String)]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy line (char '\n') <* skipSpaces <* eof
    line = (,,) <$> name <* string " would " <*> cost <* string " happiness units by sitting next to " <*> name <* char '.'
    name = munch1 isAlpha
    cost = ((id <$ string "gain ") +++ (negate <$ string "lose ")) <*> num
    num = read <$> munch1 isDigit

toGraph :: [(String, Int, String)] -> ([String], String -> String -> Int)
toGraph edges = (nodes, weight)
  where
    nodes = nub . map fst . M.keys $ weights
    weights = M.fromListWith (+) [ (k, c) | (a, c, b) <- edges, k <- [(a, b), (b, a)] ]
    weight a b = weights M.! (a, b)

solve1 :: [(String, Int, String)] -> Int
solve1 edges = maximum costs
  where
    (nodes, weight) = toGraph edges
    costs = map cost $ permutations nodes
    cost [] = 0
    cost xxs@(x:xs) = sum $ zipWith weight xxs (xs ++ [x])

solve2 :: [(String, Int, String)] -> Int
solve2 edges = maximum costs
  where
    (nodes', weight') = toGraph edges
    nodes = "" : nodes'
    weight a b | a == "" || b == "" = 0
               | otherwise = weight' a b
    costs = map cost $ permutations nodes
    cost [] = 0
    cost xxs@(x:xs) = sum $ zipWith weight xxs (xs ++ [x])

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
