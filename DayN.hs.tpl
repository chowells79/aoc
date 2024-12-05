#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "${id}"

parse :: String -> String
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $$ "Parse error: " ++ (show x)
  where
    full = munch1 (const True) <* eof

    num :: (Read a, Num a) => ReadP a
    num = read <$$> many1 (satisfy isDigit)

main :: IO ()
main = do
    inp <- parse <$$> input 1
    print inp
