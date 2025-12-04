#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.Set (Set)
import qualified Data.Set as S

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/04.txt"
         | otherwise = "example/04-" ++ show n ++ ".txt"

parse :: String -> Set (Int, Int)
parse s = S.fromList [ (r, c)
                     | (r, row) <- zip [0..] (lines s)
                     , (c, char) <- zip [0..] row
                     , char == '@'
                     ]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors p@(r, c) = filter (/= p)
    [ (r', c') | r' <- [r - 1 .. r + 1], c' <- [c - 1 .. c + 1] ]

accessible :: Set (Int, Int) -> Set (Int, Int)
accessible s = S.filter fewNeighbors s
  where
    fewNeighbors p = length (filter (`S.member` s) (neighbors p)) < 4

solve1 :: Set (Int, Int) -> Int
solve1 = S.size . accessible


solve2 :: Set (Int, Int) -> Int
solve2 = sum . map S.size . go
  where
    go s = a : if S.null a then [] else go s'
      where
        a = accessible s
        s' = s `S.difference` a

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
