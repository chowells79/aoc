#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}
{-# Language BangPatterns #-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Ord (Down(Down))
import Data.List (sortOn, tails)
import Control.Monad (guard)

import Data.Map (Map)
import qualified Data.Map.Strict as M


type Coord = (Int, Int, Int)

parse :: String -> [Coord]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = endBy coord (char '\n') <* eof
    coord = (,,) <$> num <* char ',' <*> num <* char ',' <*> num
    num = read <$> munch1 isDigit


d2 :: Coord -> Coord -> Int
d2 (x1, y1, z1) (x2, y2, z2) = dx * dx + dy * dy + dz * dz
  where
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2

kruskall :: (Ord a, Ord b) => (a -> a -> b) -> [a] -> [((a, a), Bool, [Int])]
kruskall dist cs = go (length cs) empty pairs
  where
    pairs = sortOn (uncurry dist) [ (c1, c2) | (c1:xs) <- tails cs, c2 <- xs ]

    go 1 _ _ = []
    go _ _ [] = []
    go i uf (p@(c1, c2):xs) =
        (p, altered, treeSizes uf') : go i' uf' xs
      where
        (altered, !uf') = union c1 c2 uf
        i' = if altered then i - 1 else i

solve :: [Coord] -> (Int, Int)
solve cs = (p1, p2)
  where
    ((_, _, circuits):rest) = drop 999 $ kruskall d2 cs
    p1 = product . take 3 . sortOn Down $ circuits
    p2 = x1 * x2
      where
        (((x1, _, _), (x2, _, _)), True,  _) = last rest

main :: IO ()
main = do
    (part1, part2) <- solve . parse <$> readFile "input/08.txt"
    print part1
    print part2


------------------------------------------------------------------
-- some kind of almost-efficient minimal Union-Find API
--
-- implements union-by-rank, but not path compression
--
-- has some sort of bizarre API that exists only to solve this
-- particular problem
--
-- notably:
--
-- - has a managing data structure instead of visible trees
--
-- - no explicit node creation; union creates implicitly
--
-- - no externally-accessible find
--
-- - provides a pure API for simplicity at the cost of efficiency

data P a = P !a !Int deriving (Eq, Ord, Show)
newtype UnionForest a = UF (Map a (P a)) deriving Show

empty :: UnionForest a
empty = UF M.empty

union :: Ord a => a -> a -> UnionForest a -> (Bool, UnionForest a)
union x0 x1 (UF m0)
    | v1 == v2 = (False, UF m0)
    | otherwise = (True, UF . updateRoot . updateChild $ m2)
  where
    (v1, s1, m1) = find x0 m0
    (v2, s2, m2) = find x1 m1

    (child, root) | s1 < s2 = (v1, v2)
                  | otherwise = (v2, v1)

    updateRoot = M.adjust (\(P v _) -> P v (s1 + s2)) root
    updateChild = M.adjust (\(P _ s) -> P root s) child

    find x m = case M.lookup x m of
        Just (P x' s) | x == x' -> (x', s, m)
                      | otherwise -> find x' m
        Nothing -> (x, 1, M.insert x (P x 1) m)

treeSizes :: Eq a => UnionForest a -> [Int]
treeSizes (UF m) = [ i | (a, P a' i) <- M.assocs m, a == a' ]
