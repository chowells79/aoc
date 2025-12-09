#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}
{-# Language BangPatterns #-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List (nub, sort, tails)
import Data.Bits (xor)

import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/09.txt"
         | otherwise = "example/09-" ++ show n ++ ".txt"

parse :: String -> [(Int, Int)]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = endBy pair (char '\n') <* eof
    pair = (,) <$> num <* char ',' <*> num
    num = read <$> munch1 isDigit


solve1 :: [(Int, Int)] -> Int
solve1 cs = maximum areas
  where
    areas = [ (abs dx + 1) * (abs dy + 1)
            | ((x1, y1):cs') <- tails cs
            , (x2, y2) <- cs'
            , let { dx = x1 - x2 ; dy = y1 - y2 }
            ]


compress :: [(Int, Int)] -> [(Int, Int)]
compress cs = result
  where
    (xs, ys) = unzip cs

    comp = M.fromList . go 1 . nub . sort
      where
        go i (f:ls@(s:_)) = (f, i) : go i' ls
          where
            !i' = if s == f + 1 then i + 1 else i + 2
        go i [f] = [(f, i)]

    xm = comp xs
    ym = comp ys

    result = [ (xm M.! x, ym M.! y) | (x, y) <- cs ]

findFestive :: [(Int, Int)] -> Set (Int, Int)
findFestive coords@((xf, yf):_) = S.unions [inside, hori, vert]
  where
    (hori, vert) = lines coords (S.empty, S.empty)

    lines ((x1, y1):cs@((x2, y2):_)) (!h, !v) =
        lines cs $ update x1 y1 x2 y2 h v
    lines [(x1, y1)] (h, v) = update x1 y1 xf yf h v

    update x1 y1 x2 y2 h v
        | x1 == x2 = (h, S.union v col)
        | y1 == y2 = (S.union h row, v)
      where
        xi = signum $ x2 - x1
        yi = signum $ y2 - y1

        row = S.fromList [ (x, y1) | x <- [ x1, x1 + xi .. x2 - xi ] ]
        col = S.fromList [ (x1, y) | y <- [ y1, y1 + yi .. y2 - yi ] ]

    (xs, ys) = unzip coords
    (xl, xh) = (minimum xs, maximum xs)
    (yl, yh) = (minimum ys, maximum ys)
    inside = S.fromList [ (x, y)
                        | y <- [ yl .. yh ]
                        , x <- go y xl False
                        ]
      where
        go y x i
            | x == xh = []
            | otherwise = (if i' then (x:) else id) $ go y (x + 1) i'
          where
            i' = i `xor` ((x, y) `S.member` vert)

solve2 :: [(Int, Int)] -> Int
solve2 coords = maximum areas
  where
    compressed = compress coords

    areas = [ (abs dx + 1) * (abs dy + 1)
            | (((x1, y1), c1):cs') <- tails $ zip coords compressed
            , ((x2, y2), c2) <- cs'
            , let { dx = x1 - x2 ; dy = y1 - y2 }
            , valid c1 c2
            ]

    festive = findFestive compressed
    valid (x1, y1) (x2, y2) = all (`S.member` festive) locs
      where
        locs = [ (x, y)
               | x <- [min x1 x2 .. max x1 x2]
               , y <- [min y1 y2 .. max y1 y2]
               ]


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
