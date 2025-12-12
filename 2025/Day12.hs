#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/12.txt"
         | otherwise = "example/12-" ++ show n ++ ".txt"

parse :: String -> String
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = munch (const True) <* eof

    num :: (Read a, Num a) => ReadP a
    num = read <$> munch1 isDigit

main :: IO ()
main = do
    inp <- parse <$> input 1
    print inp
