#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

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
            , singleAngle path
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
    singleAngle xs = go xs < 3
      where
        go (x:y:zs) = (if x == y then 0 else 1) + go (y:zs)
        go _ = 0



tenKey :: String -> [String]
tenKey = go 'A'
  where
    go _ [] = [[]]
    go c (x:xs) = (++) <$> tenKeyPaths M.! (c, x) <*> go x xs


directional :: String -> [String]
directional = go 'A'
  where
    go _ [] = [[]]
    go c (x:xs) = (++) <$> dirPaths M.! (c, x) <*> go x xs


score1 :: String -> Int
score1 s = read (takeWhile isDigit s) * moves s

moves :: String -> Int
moves s = head . sort . map length $ tenKey s >>= directional >>= directional


solve1 :: [String] -> Int
solve1 = sum . map score1


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
