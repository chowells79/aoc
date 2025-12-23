#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Data.List
import Text.ParserCombinators.ReadP

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/01.txt"
         | otherwise = "example/01-" ++ show n ++ ".txt"

data Dir = Down | Up deriving (Eq, Ord, Show)

parse :: String -> [Dir]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = many dir <* skipSpaces <* eof
    dir = (Down <$ char ')') +++ (Up <$ char '(')

score :: Dir -> Int
score Down = -1
score Up = 1

solve1 :: [Dir] -> Int
solve1 = sum . map score

solve2 :: [Dir] -> Int
solve2 = length . takeWhile (>= 0) . scanl' (+) 0 . map score

main :: IO ()
main = do
    inp <- parse <$> input 1
    print $ solve1 inp
    print $ solve2 inp
