#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
-}

import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Split (wordsBy)


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/06.txt"
         | otherwise = "example/06-" ++ show n ++ ".txt"

data Op = Add | Mul deriving (Eq, Ord, Show)

parse1 :: String -> [(Op, [Int])]
parse1 s = zip ops nums
  where
    (opRow:numRows) = map words . reverse . lines $ s
    nums =
        transpose .
        map (map read) $
        reverse numRows
    ops = map (\x -> case x of "+" -> Add ; "*" -> Mul) opRow

perform :: Op -> [Int] -> Int
perform Mul = product
perform Add = sum

solve :: [(Op, [Int])] -> Int
solve = sum . map (uncurry perform)


parse2 :: String -> [(Op, [Int])]
parse2 s = zip ops nums
  where
    (opRow:numRows) = reverse $ lines s
    nums =
        map (map read) .
        wordsBy (== "") .
        map (filter isDigit) .
        transpose $
        reverse numRows
    ops = map (\x -> case x of "+" -> Add ; "*" -> Mul) . words $ opRow


main :: IO ()
main = do
    inp <- input 0
    print . solve . parse1 $ inp
    print . solve . parse2 $ inp
