#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, linear
-}

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha)

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map.Lazy as M

import Linear.V4

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/11.txt"
         | otherwise = "example/11-" ++ show n ++ ".txt"

parse :: String -> [(String, [String])]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = endBy line (char '\n') <* eof
    line = (,) <$> word <* (char ':') <*> many (char ' ' *> word)
    word = munch1 isAlpha

repr :: [(String, [String])] -> Map String (Set String)
repr = M.fromList . map (fmap S.fromList)

solve1 :: Map String (Set String) -> Int
solve1 m = results M.! "you"
  where
    results = M.fromList [ (k, go k) | k <- M.keys m ]
    go k | S.member "out" s = 1
         | otherwise = sum [ results M.! k' | k' <- S.toList s ]
      where
        s = m M.! k

solve2 :: Map String (Set String) -> Int
solve2 m = let V4 both _ _ _ = results M.! "svr" in both
  where
    results = M.fromList [ (k, go k) | k <- M.keys m ]

    go k | k == "fft" = V4 (both + dac) (fft + none) 0 0
         | k == "dac" = V4 (both + fft) 0 (dac + none) 0
         | otherwise = base
      where
        base@(V4 both fft dac none)
            | S.member "out" s = V4 0 0 0 1
            | otherwise = sum [ results M.! k' | k' <- S.toList s ]

        s = m M.! k


main :: IO ()
main = do
    inp <- repr . parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
