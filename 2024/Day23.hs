#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
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


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "23"


type Graph a = Map a (Set a)

parse :: String -> Graph String
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



threeCliques :: Graph String -> Set (Set String)
threeCliques g = S.fromList $ do
    (s, e1) <- M.toList g
    (t:us) <- tails $ S.toList e1
    let tn = g M.! t
    u <- us
    guard $ S.member u tn
    pure $ S.fromList [s, t, u]

solve1 :: Graph String -> Int
solve1 = S.size . S.filter (any ("t" `isPrefixOf`)) . threeCliques


-- finds all maximal cliques in a graph
bronKerbosch :: Ord a => Graph a -> [Set a]
bronKerbosch g = go S.empty (M.keysSet g) S.empty []
  where
    go r p x | S.null p = if S.null x then (r :) else id
             | otherwise = scan r p x (g M.! S.findMin p)
    scan r p x pivotSet = case S.minView p of
        Nothing -> id
        Just (v, p') -> top . scan r p' (S.insert v x) pivotSet
          where
            top | S.member v pivotSet = id
                | otherwise = go (S.insert v r) nearP nearX
            nearP = S.intersection p nv
            nearX = S.intersection x nv
            nv = g M.! v


solve2 :: Graph String -> String
solve2 = intercalate "," . S.toList . largest . bronKerbosch
  where
    largest = last . sortBy (comparing S.size)


main :: IO ()
main = do
    g <- parse <$> input 0
    print $ solve1 g
    putStrLn $ solve2 g
