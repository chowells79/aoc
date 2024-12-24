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
    u <- us
    guard . S.member t $ g M.! u
    pure $ S.fromList [s, t, u]

solve1 :: Graph String -> Int
solve1 = S.size . S.filter (any ("t" `isPrefixOf`)) . threeCliques


-- finds all maximal cliques in a graph
simplestBronKerbosch :: Ord a => Graph a -> [Set a]
simplestBronKerbosch g = outer S.empty (M.keysSet g) S.empty []
  where
    outer r p x
        | S.null p = if S.null x then (r :) else id
        | otherwise = inner r p x
    inner r p x =
        case S.minView p of
            Nothing -> id
            Just (v, p') ->
                outer (S.insert v r) (narrow p) (narrow x) .
                inner r p' (S.insert v x)
              where
                narrow z = S.intersection z (g M.! v)


solve2 :: Graph String -> String
solve2 = intercalate "," . S.toList . largest . simplestBronKerbosch
  where
    largest = last . sortBy (comparing S.size)



main :: IO ()
main = do
    g <- parse <$> input 0
    print $ solve1 g
    putStrLn $ solve2 g
