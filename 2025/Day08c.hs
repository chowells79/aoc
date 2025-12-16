#!/usr/bin/env cabal
{- cabal:
build-depends: base, vector
-}
{-# Language RankNTypes #-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Ord (Down(Down))
import Data.List (sortOn, tails)
import Control.Monad (guard, when)

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)

import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as V

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

-- Reports a trace of running Kruskal's algorithm on a complete graph
--
-- Takes a distance function and a list of nodes
--
-- Returns a list of steps taken:
--
-- 1. The pair of nodes considered
-- 2. Whether the pair of nodes connected separate trees
-- 3. The sizes of the trees in the disjoint forest after ensuring
--    the considered nodes are in the same tree, in descending order
--
-- The output list is terminated when the spanning tree is complete.
--
-- Space invariants:
--
-- - No pattern of consumption of the result list will cause nested
--   unevaluted expressions to be built up.
kruskal :: (Ord a, Ord b) => (a -> a -> b) -> [a] -> [((a, a), Bool, [Int])]
kruskal dist cs = runST (withUnionForest cs kruskal')
  where
    kruskal' uf keyed = go (length cs) pairs
      where
        pairs = sortOn dist' [ (c1, c2) | (c1:xs) <- tails keyed, c2 <- xs ]
        dist' ((c1, _), (c2, _)) = dist c1 c2

        go 1 _ = pure []
        go _ [] = error "kruskal: Somehow ran out of edges without fully connecting the graph. Given that this takes a complete graph as input, this should be impossible"
        go i (((c1, k1), (c2, k2)):xs) = do
            altered <- union k1 k2 uf
            sizes <- forestSizes uf
            let i' = if altered then i - 1 else i
            rest <- unsafeInterleaveST $ go i' xs
            pure $ ((c1, c2), altered, sortOn Down sizes) : rest


solve :: [Coord] -> (Int, Int)
solve cs = (p1, p2)
  where
    ((_, _, circuits):rest) = drop 999 $ kruskal d2 cs
    p1 = product . take 3 $ circuits
    p2 = x1 * x2
      where
        (((x1, _, _), (x2, _, _)), True,  _) = last rest

main :: IO ()
main = do
    (part1, part2) <- solve . parse <$> readFile "input/08.txt"
    print part1
    print part2








-- STVector-based Union-Find

data UnionForest b s = UF { counts, parents :: STVector s Int }
newtype Key b = K Int


withUnionForest :: [a] -> (forall b. UnionForest b s -> [(a, Key b)] -> ST s c) -> ST s c
withUnionForest as f = do
    let l = length as
    uf <- UF <$> V.generate l (const 1) <*> V.generate l id
    let keyed = zip as (map K [0..])
    f uf keyed


union :: Key b -> Key b -> UnionForest b s -> ST s Bool
union k1 k2 uf = do
    let find (K k) = do
            p <- V.read (parents uf) k
            case k == p of
                True -> pure k
                False -> do
                    r <- find $ K p
                    -- path compression
                    when (p /= r) $ V.write (parents uf) k r
                    pure r

    i1 <- find k1
    i2 <- find k2
    case i1 == i2 of
        True -> pure False
        False -> do
            -- union by rank
            count1 <- V.read (counts uf) i1
            count2 <- V.read (counts uf) i2
            let (smaller, larger)
                    | count1 < count2 = (i1, i2)
                    | otherwise = (i2, i1)
            V.write (parents uf) smaller larger
            V.write (counts uf) larger $! count1 + count2

            pure True

forestSizes :: UnionForest b s -> ST s [Int]
forestSizes uf = V.ifoldrM rootSize [] (parents uf)
  where
    rootSize k p r | k == p = (: r ) <$> V.read (counts uf) k
                   | otherwise = pure r
