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
    total <- [ 2 .. max ]
    b <- [ 0 .. total - 1 ]
    let a = total - b
    loc <- [ (r - a, c + b), (r + b, c + a), (r + a, c - b), (r - b, c - a) ]
    pure $ (loc, total)


explore :: Loc -> Set Loc -> [Set Loc]
explore start open = go ss $ open S.\\ ss
  where
    ss = S.singleton start

    go edge avail
        | S.null edge = []
        | otherwise = edge : continue
      where
        near = S.fromList $ S.toList edge >>= neighbors
        continue = go (S.intersection near avail) (avail S.\\ near)


solve :: Maze -> Int -> Int -> Int
solve (start, end, open) cheatSize threshold = length $ winningCheats
  where
    steps = zip [0..] $ explore start open
    fair = head [ i | (i, s) <- steps, S.member end s ]
    profitable = fair - threshold

    remaining = M.fromList $ do
        (i, s) <- zip [0..] $ explore end open
        loc <- S.toList s
        pure $ (loc, i)

    winningCheats = do
        (i, s) <- takeWhile ((< profitable) . fst) steps
        loc <- S.toList s
        let skip = min cheatSize $ profitable - i
        (cheat, c) <- cheatMoves skip loc
        case M.lookup cheat remaining of
            Nothing -> []
            Just j | i + j + c <= profitable -> pure $ (loc, cheat)
                   | otherwise -> []


main :: IO ()
main = do
    maze@(start, end, open) <- parse <$> input 0
    print $ solve maze 2 100
    print $ solve maze 20 100
