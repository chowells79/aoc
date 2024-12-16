#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, psqueues
-}

import Data.Set (Set)
import qualified Data.Set as S

import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as P

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

data Dir = U | R | D | L deriving (Eq, Ord, Show)

foldDir :: a -> a -> a -> a -> Dir -> a
foldDir a b c d dir = case dir of { U -> a ; R -> b ; D -> c ; L -> d }

type State = ((Int, Int), Dir)

moves :: State -> [(Int, State)]
moves (l@(r, c), d) =
    [ (1, (forward, d))
    , (1000, (l, foldDir L U R D d))
    , (1000, (l, foldDir R D L U d))
    ]
  where
    forward = foldDir (r - 1, c) (r, c + 1) (r + 1, c) (r, c - 1) d

explore :: Maze -> [(Int, Set (Int, Int))]
explore (s, _, open) = go S.empty $ P.singleton (s, R) 0 S.empty
  where
    go seen queue = case P.minView queue of
        Nothing -> []
        Just (now, cost, visited, queue')
            | S.member now seen -> go seen queue'
            | otherwise -> (cost, visited') : go seen' queue''
          where
            visited' = S.insert (fst now) visited
            seen' = S.insert now seen
            queue'' = foldl' upsert queue'
                      [ (cost + c, next)
                      | (c, next) <- moves now
                      , S.member (fst next) open
                      ]
            upsert q (p, k) = case P.alter ups k q of (_, q') -> q'
              where
                ups (Just (p', v'))
                    | p' == p = ((), Just (p, S.union v' visited'))
                    | p' < p = ((), Just (p', v'))
                ups _ = ((), Just (p, visited'))


solve :: Maze -> (Int, Int)
solve m@(_, e, _) = fmap S.size . head . filter (S.member e . snd) $ explore m


main :: IO ()
main = do
    maze <- parse <$> input 0
    let (part1, part2) = solve maze
    print part1
    print part2
