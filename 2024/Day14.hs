#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

{-# Language BangPatterns #-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace
import Data.Foldable
import Data.Function
import Data.List


input :: Int -> IO ((Int, Int), String)
input n = (,) b <$> readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "14"
    b | n == 0 = (101, 103)
      | n == 1 = (11, 7)

data Robot = Robot !Int !Int !Int !Int deriving (Show, Eq, Ord)

parse :: String -> [Robot]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = many1 robot <* eof
    robot = do
        (px, py) <- position
        (vx, vy) <- velocity
        _ <- char '\n'
        return $! Robot px py vx vy
    position = (,) <$> findP num <*> findP num
    velocity = (,) <$> findP signedNum <*> findP signedNum

    findP p = p <++ (get *> findP p)
    signedNum = option id (negate <$ char '-') <*> num
    num = read <$> munch1 isDigit


step :: (Int, Int) -> Robot -> Robot
step (mx, my) (Robot x y vx vy) = Robot nx ny vx vy
  where
    nx = (x + vx) `mod` mx
    ny = (y + vy) `mod` my

countQuadrants :: (Int, Int) -> [Robot] -> (Int, Int, Int, Int)
countQuadrants (mx, my) = go 0 0 0 0
  where
    go !i !ii !iii !iv rs = case rs of
        [] -> (i, ii, iii, iv)
        ((Robot x y _ _):xs)
            | x < xmid && y < ymid -> go (i + 1) ii iii iv xs
            | x > xmid && y < ymid -> go i (ii + 1) iii iv xs
            | x < xmid && y > ymid -> go i ii (iii + 1) iv xs
            | x > xmid && y > ymid -> go i ii iii (iv + 1) xs
            | otherwise -> go i ii iii iv xs
    xmid = mx `div` 2
    ymid = my `div` 2

safetyFactor :: (Int, Int) -> [Robot] -> Int
safetyFactor b r = case countQuadrants b r of
    (a, b, c, d) -> a * b * c * d

solve1 :: (Int, Int) -> [Robot] -> Int
solve1 b rs = safetyFactor b $ allStates b rs !! 100


runForever :: (Int, Int) -> [Robot] -> [[Robot]]
runForever = iterate . map . step

allStates :: (Int, Int) -> [Robot] -> [[Robot]]
allStates b = go S.empty . runForever b
  where
    go s (r:rs)
        | S.member r s = []
        | otherwise = r : go (S.insert r s) rs


treeness :: (Int, Int) -> [Robot] -> Int
treeness (mx, my) rs = sum [ d x y | Robot x y _ _ <- rs ]
  where
    d x y = abs (x - xmid) + abs (y - ymid)
    xmid = mx `div` 2
    ymid = my `div` 2


draw :: (Int, Int) -> [Robot] -> IO ()
draw (mx, my) rs = do
    let s = S.fromList [ (x, y) | Robot x y _ _ <- rs ]
    for_ [0 .. my - 1] $ \row -> do
        for_ [0 .. mx - 1] $ \col -> do
            putStr $ if S.member (col, row) s then "X" else " "
        putStrLn ""


solve2 :: (Int, Int) -> [Robot] -> Int
solve2 b rs = fst . head $ candidates
  where
    candidates = sortOn (treeness b . snd) . zip [0..] . allStates b $ rs


main :: IO ()
main = do
    (bounds, s) <- input 0
    let inp = parse s
    print $ solve1 bounds inp

    print $ solve2 bounds inp
