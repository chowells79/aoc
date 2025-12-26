#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, chr)

import Numeric (readHex)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/08.txt"
         | otherwise = "example/08-" ++ show n ++ ".txt"

unliteral :: String -> Maybe String
unliteral ('"':s) = go s
  where
    go [] = Nothing
    go ['"'] = Just ""
    go ('\\':'"':cs) = ('"' :) <$> go cs
    go ('\\':'\\':cs) = ('\\' :) <$> go cs
    go ('\\':'x':c1:c2:cs) = (:) <$> c <*> go cs
      where
        c = case readHex [c1, c2] of
              [(i, "")] -> Just $ chr i
              _ -> Nothing
    go ('\\':_) = Nothing
    go (c:cs) = (c :) <$> go cs
unliteral _ = Nothing

solve1 :: [String] -> Int
solve1 strs = lits - unlits
  where
    lits = sum . map length $ strs
    unlits = sum . map (maybe 0 length . unliteral) $ strs

toLiteral :: String -> String
toLiteral = ('"' :) . go
  where
    go [] = "\""
    go ('\\':cs) = '\\' : '\\' : go cs
    go ('"':cs) = '\\' : '"' : go cs
    go (c:cs) = c : go cs

solve2 :: [String] -> Int
solve2 strs = lits - unlits
  where
    lits = sum . map (length . toLiteral) $ strs
    unlits = sum . map length $ strs

main :: IO ()
main = do
    inp <- lines <$> input 0
    print $ solve1 inp
    print $ solve2 inp
