#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.Ord
import Data.List

import Data.Char (isDigit)

import Data.Map (Map)
import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "21"


parse :: String -> [String]
parse s = lines s

tenKeyPaths :: Map (Char, Char) [String]
tenKeyPaths = fromLayout ["789","456","123"," 0A"]

dirPaths :: Map (Char, Char) [String]
dirPaths = fromLayout [" ^A", "<v>"]

fromLayout :: [String] -> Map (Char, Char) [String]
fromLayout lines = M.fromListWith (++) paths
  where
    coords = M.fromList
        [ ((row, col), char)
        | (row, line) <- zip [0..] lines
        , (col, char) <- zip [0..] line
        , char /= ' '
        ]
    paths = [ ((coords M.! start, end), [path])
            | (loc, end) <- M.toList coords
            , shell <- explore loc coords
            , (start, path) <- shell
            ]
    explore end = go [(end, "A")] . M.delete end
      where
        go edge avail
            | null edge = []
            | otherwise = edge : go next avail'
          where
            next = [ (n, d:p)
                   | (loc, p) <- edge
                   , (n, d) <- neighbors loc
                   , M.member n avail
                   ]
            avail' = foldl' (\m (k, _) -> M.delete k m) avail next
    neighbors (r, c) = [ ((r - 1, c), 'v'), ((r, c + 1), '<')
                       , ((r + 1, c), '^'), ((r, c - 1), '>')
                       ]


cost :: Int -> String -> Int
cost directionals str = result
  where
    result = minimum . map (sum . map (full directionals) . expand) $ tenKey str
    expand xs = zip ('A':xs) xs

    tenKey = go 'A'
      where
        go _ [] = [[]]
        go c (x:xs) = (++) <$> tenKeyPaths M.! (c, x) <*> go x xs

    full 0 _ = 1
    full n m =
        minimum . map (sum . map (memo (n - 1)) . expand) $ dirPaths M.! m

    memo n (s, t) = table M.! (n, s, t)
    table = M.fromList [ ((n, s, t), full n (s, t))
                       | n <- [0 .. directionals ]
                       , s <- "<>v^A"
                       , t <- "<>v^A"
                       ]


score :: Int -> String -> Int
score n s = read (takeWhile isDigit s) * cost n s

solve :: Int -> [String] -> Int
solve n = sum . map (score n)

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve 2 inp
    print $ solve 25 inp
