#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, psqueues, hashable
-}
{-# Language DeriveAnyClass, DeriveFunctor, DeriveGeneric #-}

import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.HashPSQ as P

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "16"

type Maze = ((Int, Int), (Int, Int), Set (Int, Int))

parse :: String -> Maze
parse s = (start, end, open)
  where
    raw = [ ((row, col), c)
          | (row, line) <- zip [0..] $ lines s
          , (col, c) <- zip [0..] line
          ]
    open = S.fromList . map fst . filter ((/= '#') . snd) $ raw
    start = fst . head . filter ((== 'S') . snd) $ raw
    end = fst . head . filter ((== 'E') . snd) $ raw

data Dir = N | E | S | W deriving (Eq, Ord, Show, Generic, Hashable)

foldDir :: a -> a -> a -> a -> Dir -> a
foldDir a b c d dir = case dir of { N -> a ; E -> b ; S -> c ; W -> d }

type State = ((Int, Int), Dir)

moves :: State -> [(Int, State)]
moves (l@(r, c), d) =
    [ (1, (forward, d))
    , (1000, (l, foldDir E S W N d))
    , (1000, (l, foldDir W N E S d))
    ]
  where
    forward = foldDir (r - 1, c) (r, c + 1) (r + 1, c) (r, c - 1) d

data Infinite a = Finite a | Infinity deriving (Eq, Ord, Show, Functor)

explore :: Maze -> [(Infinite Int, Set (Int, Int))]
explore (s, _, open) = go universe
  where
    universe = P.insert (s, E) (Finite 0) S.empty $ P.fromList
        [ ((l, d), Infinity, S.empty)
        | l <- S.toList open
        , d <- [N, E, S, W]
        ]
    go queue = case P.minView queue of
        Nothing -> []
        Just (now, cost, visited, queue') -> (cost, visited') : go queue''
          where
            visited' = S.insert (fst now) visited
            queue'' = foldl' combine queue'
                      [ (fmap (+ c) cost, next)
                      | (c, next) <- moves now
                      ]
            combine q (p, k) = snd $ P.alter ((,) () . fmap dec) k q
              where
                dec (p', v')
                    | p < p' = (p, visited')
                    | p == p' = (p, S.union v' visited')
                    | otherwise = (p', v')


solve :: Maze -> (Infinite Int, Int)
solve m@(_, e, _) = (minCost, S.size $ S.unions allDirs)
  where
    all = explore m
    found = dropWhile (S.notMember e . snd) all
    minCost = fst $ head found
    allDirs = filter (S.member e) . map snd .
              takeWhile ((== minCost) . fst) $ found


main :: IO ()
main = do
    maze <- parse <$> input 0
    let (part1, part2) = solve maze
    print part1
    print part2
