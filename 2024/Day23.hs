#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, maximal-cliques
-}

import Control.Monad
import Data.List
import Data.Ord

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S


import qualified Data.Algorithm.MaximalCliques as MC


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "23"


type Graph = Map String (Set String)

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


threeCliques :: Graph -> Set (Set String)
threeCliques g = S.fromList $ do
    (s, e1) <- M.toList g
    t <- S.toList e1
    let e2 = g M.! t
        thirds = S.intersection e1 e2
    u <- S.toList thirds
    pure $ S.fromList [s, t, u]

solve1 :: Graph -> Int
solve1 = S.size . S.filter (any ("t" `isPrefixOf`)) . threeCliques



edge :: Graph -> String -> String -> Bool
edge g s t = S.member s $ g M.! t

mergeTCliques :: Graph -> Set (Set String) -> Set (Set String)
mergeTCliques g xs = S.fromList $ do
    (x:ys) <- tails $ S.toList xs
    y <- ys
    guard $ S.size (S.intersection x y) == S.size x - 1
    let n1 = S.findMin $ x S.\\ y
    let n2 = S.findMin $ y S.\\ x
    guard $ edge g n1 n2
    let u = S.union x y
    guard $ any ("t" `isPrefixOf`) u
    pure u

solve2 :: Graph -> String
solve2 g = format . last . go . threeCliques $ g
  where
    go s | S.null s = []
         | otherwise = s : go (mergeTCliques g s)
    format = intercalate "," . S.toList . S.findMin


solve2a :: Graph -> String
solve2a g = intercalate "," . largest . hasT $ cliques
  where
    hasT = filter $ any ("t" `isPrefixOf`)
    largest = last . sortBy (comparing length)
    cliques = MC.getMaximalCliques (edge g) (S.toList $ M.keysSet g)

main :: IO ()
main = do
    g <- parse <$> input 0
    print $ solve1 g
    putStrLn $ solve2a g
    putStrLn $ solve2 g

