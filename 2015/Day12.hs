#!/usr/bin/env cabal
{- cabal:
build-depends: base, lens, aeson, lens-aeson
-}
{-# Language OverloadedStrings #-}

import Data.Aeson (Value)

import Control.Lens
import Data.Aeson.Lens

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/12.txt"
         | otherwise = "example/12-" ++ show n ++ ".txt"

parse :: String -> Value
parse = view $ singular _JSON

solve1 :: Value -> Integer
solve1 = sumOf $ cosmos . _Integer

solve2 :: Value -> Integer
solve2 = sumOf $ cosmosOf (filtered (not . red) . plate) . _Integer
  where
    red = anyOf (members . _String) (== "red")

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
