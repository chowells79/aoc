#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit)

import Data.Map (Map)
import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/16.txt"
         | otherwise = "example/16-" ++ show n ++ ".txt"

parse :: String -> [(Int, Map String Int)]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy line (char '\n') <* skipSpaces <* eof
    line = (,) <$ string "Sue " <*> num <* string ": " <*> attrs
    attrs = M.fromList <$> sepBy pair (string ", ")
    pair = (,) <$> munch1 isAlpha <* string ": " <*> num
    num = read <$> munch1 isDigit


target1 :: Map String Int
target1 = M.fromList
    [ ("children", 3)
    , ("cats", 7)
    , ("samoyeds", 2)
    , ("pomeranians", 3)
    , ("akitas", 0)
    , ("vizslas", 0)
    , ("goldfish", 5)
    , ("trees", 3)
    , ("cars", 2)
    , ("perfumes", 1)
    ]

solve1 :: [(Int, Map String Int)] -> [Int]
solve1 = map fst . filter (and . M.intersectionWith (==) target1 . snd)


target2 :: Map String (Int -> Bool)
target2 = M.fromList
    [ ("children", (== 3))
    , ("cats", (> 7))
    , ("samoyeds", (== 2))
    , ("pomeranians", (< 3))
    , ("akitas", (== 0))
    , ("vizslas", (== 0))
    , ("goldfish", (< 5))
    , ("trees", (> 3))
    , ("cars", (== 2))
    , ("perfumes", (== 1))
    ]

solve2 :: [(Int, Map String Int)] -> [Int]
solve2 = map fst . filter (and . M.intersectionWith id target2 . snd)


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
