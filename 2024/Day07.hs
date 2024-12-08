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
    ident = "07"

parse :: String -> [(Int, [Int])]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = endBy1 line (char '\n') <* eof
    line = (,) <$> num <* (string ": ") <*> sepBy1 num (char ' ')
    num = read <$> many1 (satisfy isDigit)

valid :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
valid _ _ [] = False
valid ops target (x:xs) = foldr test (== target) xs x
  where
    test y go acc
        | acc > target = False
        | otherwise = or [ go (acc <#> y) | (<#>) <- ops ]

solve :: [Int -> Int -> Int] -> [(Int, [Int])] -> Int
solve ops xs = sum [ target | (target, xs) <- xs , valid ops target xs ]

ops1 :: [Int -> Int -> Int]
ops1 = [(+), (*)]

ops2 :: [Int -> Int -> Int]
ops2 = [(+), (*), \a b -> read $ show a ++ show b]


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve ops1 inp
    print $ solve ops2 inp
