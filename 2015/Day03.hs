#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, split
-}
{-# Language BangPatterns #-}

import Data.List
import Data.List.Split (chunksOf)

import Text.ParserCombinators.ReadP

import Data.Set (Set)
import qualified Data.Set as S

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/03.txt"
         | otherwise = "example/03-" ++ show n ++ ".txt"

data Dir = N | S | E | W deriving (Eq, Ord, Show)

parse :: String -> [Dir]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = many dir <* skipSpaces <* eof
    dir = (N <$ char '^') +++ (S <$ char 'v') +++
          (E <$ char '>') +++ (W <$ char '<')

move :: (Int, Int) -> Dir -> (Int, Int)
move (!x, !y) d = (x + dx, y + dy)
  where
    (dx, dy) = case d of
                 N -> (0, 1)
                 S -> (0, -1)
                 E -> (1, 0)
                 W -> (-1, 0)

visited :: [Dir] -> Set (Int, Int)
visited = S.fromList . scanl' move (0, 0)

solve1 :: [Dir] -> Int
solve1 = S.size . visited

solve2 :: [Dir] -> Int
solve2 ds = S.size $ visited as `S.union` visited bs
  where
    [as, bs] = transpose $ chunksOf 2 ds


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
