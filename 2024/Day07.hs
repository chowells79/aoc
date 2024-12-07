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

totals1 :: [Int] -> [Int]
totals1 [] = error "totals called on empty list"
totals1 (x:xs) = go x xs []
  where
    -- go acc _ | acc > target = id
    go acc [] = (acc :)
    go acc (y:ys) = go (acc + y) ys . go (acc * y) ys

valid1 :: Int -> [Int] -> Bool
valid1 target xs = target `elem` totals1 xs

solve1 :: [(Int, [Int])] -> Int
solve1 xs = sum
    [ target
    | (target, xs) <- xs
    , valid1 target xs
    ]

totals2 :: [Int] -> [Int]
totals2 [] = error "totals called on empty list"
totals2 (x:xs) = go x xs []
  where
    -- go acc _ | acc > target = id
    go acc [] = (acc :)
    go acc (y:ys) = go (acc + y) ys
        . go (acc * y) ys .
        go (read $ show acc ++ show y) ys

valid2 :: Int -> [Int] -> Bool
valid2 target xs = target `elem` totals2 xs

solve2 :: [(Int, [Int])] -> Int
solve2 xs = sum
    [ target
    | (target, xs) <- xs
    , valid2 target xs
    ]

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
