#!/usr/bin/env cabal
{- cabal:
build-depends: base, matrix
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import qualified Data.Matrix as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/15.txt"
         | otherwise = "example/15-" ++ show n ++ ".txt"

data Ingredient = Ingredient
    { name :: String
    , capacity, durability, flavor, texture, calories :: Int
    }
    deriving (Eq, Ord, Show)

parse :: String -> [Ingredient]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy line (char '\n') <* skipSpaces <* eof
    line = Ingredient <$> na <*> cap <*> dur <*> fla <*> tex <*> cal
    na = munch1 (\x -> x /= ':') <* char ':'
    cap = string " capacity " *> snum <* char ','
    dur = string " durability " *> snum <* char ','
    fla = string " flavor " *> snum <* char ','
    tex = string " texture " *> snum <* char ','
    cal = string " calories " *> snum
    snum = read <$> munch1 (\x -> isDigit x || x == '-')


totals :: Int -> Int -> [[Int]]
totals i 1 = [[i]]
totals i n = do
    j <- [0 .. i]
    js <- totals (i - j) (n - 1)
    pure $ j:js

solve1 :: [Ingredient] -> Int
solve1 is = maximum scores
  where
    arrangements = totals 100 $ length is
    multipliers = M.fromLists [ [c, d, f, t] | Ingredient _ c d f t _ <- is ]
    scores = map (\a -> product . fmap (max 0) $ M.fromLists [a] * multipliers) arrangements

solve2 :: [Ingredient] -> Int
solve2 is = maximum scores
  where
    arrangements = totals 100 $ length is
    multipliers = M.fromLists [ [c, d, f, t, c']
                              | Ingredient _ c d f t c' <- is ]
    scores = concatMap xx arrangements
    xx a = x . M.toList . fmap (max 0) $ M.fromLists [a] * multipliers
    x a | last a == 500 = [product $ init a]
        | otherwise = []

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
