#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Set as S

import Data.Maybe


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "06"


data Space = Unvisited | Visited | Obstacle
    deriving (Eq, Ord, Show)

data Guard = Guard Int Int Facing
    deriving (Eq, Ord, Show)

data Facing = U | R | D | L
    deriving (Eq, Ord, Show)

type Board = Map (Int, Int) Space

parse :: String -> (Board, Guard)
parse s = (M.fromList b, g)
  where
    raw = [ (row, col, examineChar char)
          | (row, line) <- zip [1..] (lines s)
          , (col, char) <- zip [1..] line
          ]
    g = head [ Guard row col f | (row, col, (_, Just f)) <- raw ]
    b = [ ((row, col), s) | (row, col, (s, _)) <- raw ]

    examineChar '.' = (Unvisited, Nothing)
    examineChar '#' = (Obstacle, Nothing)
    examineChar '^' = (Visited, Just U)
    examineChar '>' = (Visited, Just R)
    examineChar 'v' = (Visited, Just D)
    examineChar '<' = (Visited, Just L)
    examineChar c = error $ "Unexpected character: " ++ [c]


step1 :: Board -> Guard -> (Board, Maybe Guard)
step1 board (Guard row col facing)
    | next == Just Obstacle = (board, Just (Guard row col (rotate facing)))
    | next == Nothing = (board, Nothing)
    | otherwise = (board', Just (Guard row' col' facing))
  where
    (row', col') = case facing of
                     U -> (row - 1, col)
                     R -> (row, col + 1)
                     D -> (row + 1, col)
                     L -> (row, col - 1)
    next = M.lookup (row', col') board
    board' = M.insert (row', col') Visited board

    rotate U = R
    rotate R = D
    rotate D = L
    rotate L = U

run1 :: Board -> Guard -> Board
run1 b g = case step1 b g of
             (b', Nothing) -> b'
             (b', Just g') -> run1 b' g'

solve1 :: Board -> Guard -> Int
solve1 b g = sum [ 1 | (_, Visited) <- M.toList (run1 b g) ]


loops2 :: Board -> Guard -> Bool
loops2 = go S.empty
  where
    go prev board guard
        | S.member guard prev = True
        | otherwise = case guard' of
            Nothing -> False
            Just g -> go (S.insert guard prev) board' g
      where
        (board', guard') = step1 board guard


solve2 :: Board -> Guard -> Int
solve2 b g@(Guard row col _) = sum
    [ 1
    | (l, Visited) <- M.toList (run1 b g)
    , l /= (row, col)
    , loops2 (M.insert l Obstacle b) g
    ]


main :: IO ()
main = do
    (board, guard) <- parse <$> input 0
    print $ solve1 board guard
    print $ solve2 board guard
