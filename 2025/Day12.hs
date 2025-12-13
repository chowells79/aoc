#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List (elemIndices)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/12.txt"
         | otherwise = "example/12-" ++ show n ++ ".txt"

parse :: String -> (Map Int (Set (Int, Int)), [((Int, Int), [Int])])
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = (,) <$> boxes <*> regions <* eof

    boxes = M.fromList <$> many box
    box = (,) <$> index <*> shape
    index = num <* string ":\n"
    shape = S.fromList . coords '#' <$> manyTill (manyTill get nl) nl

    coords c xs = [ (i, j) | (row, i) <- zip xs [0..], j <- elemIndices c row ]

    regions = endBy region nl
    region = (,) <$> dimensions <*> counts
    dimensions = (,) <$> num <* char 'x' <*> num <* string ": "
    counts = sepBy num (char ' ')

    nl = char '\n'
    num = read <$> munch1 isDigit

solve1 :: (Map Int (Set (Int, Int)), [((Int, Int), [Int])]) -> Int
solve1 (boxes, regions) = length fitting
  where
    fitting = filter fits regions
    fits ((i, j), xs) =
        i * j >= sum [ c * S.size (boxes M.! n) | (n, c) <- zip [0..] xs]

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
