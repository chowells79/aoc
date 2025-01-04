#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.List (intercalate, isPrefixOf, sort, sortBy)
import Data.Ord (comparing)

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S



import Control.Monad (guard)
import Data.Foldable (toList)
import Data.List (tails)

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- | Returns a list of all maximal cliques in the input graph. Neither
-- the cliques nor the elements within each clique are returned in any
-- particular order.
maximalCliques
    :: Foldable f
    => f a -- ^ a collection of all nodes in the graph
    -> (a -> a -> Bool) -- ^ whether an edge exists between two nodes
    -> [[a]]
maximalCliques nodes edge = map (ns IM.!) <$> bronKerbosch [] graph IS.empty []
  where
    ns = IM.fromList $ zip [0..] (toList nodes)
    graph = IM.fromListWith IS.union $ do
        ((i, m):jns) <- tails $ IM.toList ns
        (j, n) <- jns
        guard $ edge m n
        [ (i, IS.singleton j), (j, IS.singleton i) ]

-- A version of the Bron-Kerbosch algorithm adapted from
-- https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm with
-- data structure choices modified for functionality. Returns a
-- difference list ([X] -> [X]) to allow efficiently concatenating
-- result sublists and streaming consumption. Note that result order
-- is not specified in any way.
--
-- Prefer the maximalCliques wrapper whenever possible. It's
-- generally nicer to use.
--
-- Initial conditions:
--
-- 1. r should be []
--
-- 2. x0 should be IS.empty
--
-- 3. p0 should be an adjacency map for the graph. The keys of p0 are
--    the nodes in the graph, and the value associated with each key
--    is the set of nodes adjacent to that key.
--
-- Invariants preserved over all recursive calls:
--
-- 1. the elements of r form a clique
--
-- 2. the keys of p0 are disjoint from the elements of x0
--
-- 3. the keys of p0 unioned with the elements of x0 make up all the
--    common neighbors of all elements in r
--
-- 4. the elements in x0 have previously been explored as additions to
--    r and do not need to be considered again
bronKerbosch :: [Int] -> IntMap IntSet -> IntSet -> [[Int]] -> [[Int]]
bronKerbosch r p0 x0 = case IM.lookupMin p0 of
    Nothing -> if IS.null x0 then (r :) else id
    Just (_, pivotSet) -> foldr step (const $ const id) (IM.toList p0) p0 x0
      where
        step (v, neighbors) loop p x
            | IS.member v pivotSet = loop p x
            | otherwise = addV . skipV
          where
            addV = p' `seq` x' `seq` bronKerbosch (v : r) p' x'
              where
                p' = IM.restrictKeys p neighbors
                x' = IS.intersection x neighbors
            skipV = p' `seq` x' `seq` loop p' x'
              where
                p' = IM.delete v p
                x' = IS.insert v x



input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "23"

parse :: String -> Map String (Set String)
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = toGraph <$> edges <* eof
    edges = endBy1 edge (char '\n')
    edge = (,) <$> munch1 isAlpha <* char '-' <*> munch1 isAlpha

    toGraph l = M.fromListWith S.union $ do
        (x, y) <- l
        [ (x, S.singleton y), (y, S.singleton x) ]



threeCliques :: Map String (Set String) -> Set (Set String)
threeCliques g = S.fromList $ do
    (s, e1) <- M.toList g
    (t:us) <- tails $ S.toList e1
    let tn = g M.! t
    u <- us
    guard $ S.member u tn
    pure $ S.fromList [s, t, u]


solve1 :: Map String (Set String) -> Int
solve1 = S.size . S.filter (any ("t" `isPrefixOf`)) . threeCliques

solve2 :: Map String (Set String) -> String
solve2 g = intercalate "," . sort . last . sortBy (comparing length) $ maxes
  where
    maxes = maximalCliques (M.keysSet g) (\s t -> S.member s $ g M.! t)


main :: IO ()
main = do
    g <- parse <$> input 0
    print $ solve1 g
    putStrLn $ solve2 g
