#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Set (Set)
import qualified Data.Set as S

import Data.List

import Debug.Trace

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "18"

parse :: String -> [(Int, Int)]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = endBy1 coord (char '\n') <* eof
    coord = (,) <$> num <* char ',' <*> num
    num = read <$> munch1 isDigit

available :: [(Int, Int)] -> Set (Int, Int)
available xs = universe S.\\ S.fromList xs
  where
    universe = S.fromList $ liftA2 (,) [0..70] [0..70]


explore :: (Int, Int) -> Set (Int, Int) -> [Set (Int, Int)]
explore = go . S.singleton
  where
    go edge avail
        | S.null edge = []
        | otherwise = edge : go (S.intersection neighbors avail) (avail S.\\ neighbors)
      where
        neighbors = S.fromList $ S.toList edge >>= \(x, y) ->
            [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]


solve1 :: [(Int, Int)] -> Int
solve1 xs = fst dest
  where
    avail = available $ take 1024 xs
    seen = explore (0, 0) avail
    layers = zip [0..] seen
    dest = head $ filter (S.member (70, 70) . snd) layers


solve2 :: [(Int, Int)] -> (Int, Int)
solve2 xs = head [ xs !! (i - 1) | i <- [1024 ..], not $ reachable i ]
  where
    reachable i = any (S.member (70, 70)) . explore (0,0) .
        available $ take i xs


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
