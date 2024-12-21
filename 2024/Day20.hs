#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Control.Monad
import Data.List
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "20"

type Loc = (Int, Int)
type Maze = (Loc, Loc, Set Loc)

parse :: String -> Maze
parse s = (start, end, open)
  where
    raw = [ (row, col, char)
          | (row, line) <- zip [0..] $ lines s
          , (col, char) <- zip [0..] line
          , char /= '#'
          ]
    open = S.fromList [ (r, c) | (r, c, _) <- raw ]
    start = head [ (r, c) | (r, c, 'S') <- raw ]
    end = head [ (r, c) | (r, c, 'E') <- raw ]


neighbors :: Loc -> [Loc]
neighbors (r, c) = [ (r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1) ]


cheatMoves :: Int -> Loc -> [(Loc, Int)]
cheatMoves max (r, c) = do
    cost <- [ 2 .. max ]
    b <- [ 0 .. cost - 1 ]
    let a = cost - b
    loc <- [ (r - a, c + b), (r + b, c + a), (r + a, c - b), (r - b, c - a) ]
    pure (loc, cost)


explore :: Loc -> Set Loc -> [Set Loc]
explore start open = go ss $ open S.\\ ss
  where
    ss = S.singleton start

    go edge avail
        | S.null edge = []
        | otherwise = edge : go edge' avail'
      where
        edge' = S.intersection avail near
        avail' = S.difference avail near
        near = S.fromList $ neighbors =<< S.toList edge


solve :: Maze -> Int -> Int -> Int
solve (start, end, open) cheatSize threshold = length $ winningCheats
  where
    steps = zip [0..] $ explore start open
    fair = head [ soFar | (soFar, edge) <- steps, S.member end edge ]
    profitable = fair - threshold

    remaining = M.fromList $ do
        (remains, edge) <- zip [0..] $ explore end open
        loc <- S.toList edge
        pure (loc, remains)

    winningCheats = do
        (soFar, edge) <- steps
        loc <- S.toList edge
        let maxCost = min cheatSize (profitable - soFar)
        (cheat, cost) <- cheatMoves maxCost loc
        remains <- maybeToList $ M.lookup cheat remaining
        guard $ soFar + remains + cost <= profitable
        pure (loc, cheat)


main :: IO ()
main = do
    maze <- parse <$> input 0
    print $ solve maze 2 100
    print $ solve maze 20 100
