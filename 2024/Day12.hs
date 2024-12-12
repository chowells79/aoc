#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "12"

parse :: String -> Map (Int, Int) Char
parse s = M.fromList
    [ ((row, col), char)
    | (row, line) <- zip [1..] $ lines s
    , (col, char) <- zip [1..] $ line
    ]

regions :: Map (Int, Int) Char -> [Set (Int, Int)]
regions m = case M.minViewWithKey m of
    Nothing -> []
    Just ((loc, char), m') -> case grow char m' loc of
        (m'', region) -> region : regions m''

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]

grow
    :: Char
    -> Map (Int, Int) Char
    -> (Int, Int)
    -> (Map (Int, Int) Char, Set (Int, Int))
grow char m l = (m `M.withoutKeys` final, final)
  where
    final = go S.empty $ S.singleton l
    go seen next
        | S.null next = seen
        | otherwise = go (S.union seen next) $ S.fromList
                      [ x
                      | frontier <- toList next
                      , x <- neighbors frontier
                      , M.lookup x m == Just char
                      , S.notMember x seen
                      ]


area :: Set (Int, Int) -> Int
area = S.size

perimeter :: Set (Int, Int) -> Int
perimeter s = sum [ 1
                  | loc <- toList s
                  , next <- neighbors loc
                  , S.notMember next s
                  ]

price1 :: Set (Int, Int) -> Int
price1 s = area s * perimeter s

solve1 :: Map (Int, Int) Char -> Int
solve1 = sum . map price1 . regions


sides :: Set (Int, Int) -> Int
sides s = length samples
  where
    pieces = S.fromList
        [ (loc, dir)
        | loc <- toList s
        , (dir, next) <- zip [1..] $ neighbors loc
        , S.notMember next s
        ]
    samples =
        [ ()
        | p@(loc, dir) <- toList pieces
        , let ns = S.fromList $ zip (neighbors loc) (repeat dir)
              adj = S.intersection pieces ns
          in Just p > S.lookupMax adj
        ]

price2 :: Set (Int, Int) -> Int
price2 s = area s * sides s

solve2 :: Map (Int, Int) Char -> Int
solve2 = sum . map price2 . regions


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
