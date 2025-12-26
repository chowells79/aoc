#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit)

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (nub, permutations)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/09.txt"
         | otherwise = "example/09-" ++ show n ++ ".txt"

parse :: String -> [(String, String, Int)]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy line (char '\n') <* skipSpaces <* eof
    line = (,,) <$> loc <* string " to " <*> loc <* string " = " <*> num
    loc = munch1 isAlpha
    num = read <$> munch1 isDigit

graphize
    :: [(String, String, Int)]
    -> ([String], String -> String -> Maybe Int)
graphize edges = (nub allLocs, weight)
  where
    allLocs = concatMap (\(a, b, _) -> [a, b]) edges
    weights = M.fromList [ (p, w) | (a, b, w) <- edges, p <- [(a, b), (b, a)] ]
    weight a b = M.lookup (a, b) weights

solve :: ([Int] -> Int) -> [(String, String, Int)] -> Int
solve select edges = select costs
  where
    (nodes, weight) = graphize edges
    costs = [ cost perm | perm <- permutations nodes ]
    cost ls = maybe maxBound sum . sequence $ zipWith weight ls (tail ls)

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve minimum inp
    print $ solve maximum inp
