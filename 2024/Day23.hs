#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.List (intercalate, isPrefixOf, sortBy)
import Data.Ord

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

-- | Returns a list of all maximal cliques in the input graph. The
-- cliques are not returned in any particular order, but the elements
-- in each clique are returned in the same order that they are folded
-- over in the input collection
maximalCliques
    :: Foldable f
    => f a -- ^ a collection of all nodes in the graph
    -> (a -> a -> Bool) -- ^ whether an edge exists between two nodes
    -> [[a]]
maximalCliques nodes edge = map (ns IM.!) <$> bronKerbosch id graph IS.empty []
  where
    ns = IM.fromList $ zip [0..] (toList nodes)
    graph = IM.fromListWith IS.union $ do
        ((i, m):jns) <- tails $ IM.toList ns
        (j, n) <- jns
        guard $ edge m n
        [ (i, IS.singleton j), (j, IS.singleton i) ]

bronKerbosch
    :: ([Int] -> [Int]) -- ^ CPSed clique element accumulator
    -> IntMap IntSet -- ^ unvisited nodes and their adjacency sets
    -> IntSet -- ^ excluded nodes, visited in earlier passes
    -> [[Int]] -> [[Int]] -- ^ CPSed list of cliques
bronKerbosch r p0 x0
    | IM.null p0 = if IS.null x0 then (r [] :) else id
    | otherwise = go p0 x0
  where
    pivotSet = snd $ IM.findMin p0
    go p x = case IM.minViewWithKey p of
        Nothing -> id
        Just ((v, neighbors), p')
            | IS.member v pivotSet -> go p' x
            | otherwise -> addV . go p' (IS.insert v x)
          where
            addV = bronKerbosch (r . (v :)) nearP nearX
            nearP = IM.restrictKeys p neighbors
            nearX = IS.intersection x neighbors




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
solve2 g = intercalate "," . last . sortBy (comparing length) $ maxes
  where
    maxes = maximalCliques (M.keysSet g) (\s t -> S.member s $ g M.! t)


main :: IO ()
main = do
    g <- parse <$> input 0
    print $ solve1 g
    putStrLn $ solve2 g
