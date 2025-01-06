#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Either
import Data.List


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "25"


type I = ([Lock], [Key])
data Lock = L [Int] deriving (Eq, Ord, Show)
data Key = K [Int] deriving (Eq, Ord, Show)

parse :: String -> I
parse s = case readP_to_S blocks s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    blocks = partitionEithers <$> sepBy1 (lock +++ key) (char '\n') <* eof

    lock = Left <$> lockBlock
    key = Right <$> keyBlock

    full = count 5 (char '#') <* char '\n'
    empty = count 5 (char '.') <* char '\n'
    mixed = count 5 (char '.' +++ char '#') <* char '\n'

    lockBlock = L . c <$ full <*> count 5 mixed <* empty
    keyBlock = K . c <$ empty <*> count 5 mixed <* full

    c = map (length . filter (== '#')) . transpose


solve1 :: I -> Int
solve1 (ls, ks) = sum
    [ 1
    | L l <- ls
    , K k <- ks
    , all (<6) $ zipWith (+) l k
    ]


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
