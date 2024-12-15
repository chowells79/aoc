#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Map (Map)
import qualified Data.Map as M

import Control.Applicative ((<|>))
import Data.Bifunctor
import Data.Maybe

import Data.Foldable

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "15"


data Direction = U | R | D | L deriving (Eq, Ord, Show)
data Tile = Empty | Wall | Box | LB | RB deriving (Eq, Ord, Show)
data Board = Board (Int, Int) !(Map (Int, Int) Tile) deriving (Eq, Ord, Show)


parse :: String -> (Board, [Direction])
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = (,) <$> board <* char '\n' <*> dirs <* eof

    board = fromGrid <$> grid dTile

    fromGrid dm = Board r m
      where
        r = head [ l | (l, Nothing) <- M.toList dm ]
        m = M.fromList [ (l, fromMaybe Empty dt) | (l, dt) <- M.toList dm ]

    grid p = M.fromList . addCoord <$> endBy1 (gline p) (char '\n')
    gline p = zip [0..] <$> many1 p

    addCoord = concat . zipWith (\i -> map (first ((,) i))) [0..]

    dTile = Nothing <$ char '@' <|> Just <$> tile
    tile = Empty <$ char '.' <|> Wall <$ char '#' <|> Box <$ char 'O'

    dirs = manyTill (dir <* skipSpaces) eof
    dir = U <$ char '^' <|>
          R <$ char '>' <|>
          D <$ char 'v' <|>
          L <$ char '<'

step :: Direction -> Board -> Board
step dir (Board l0 m0) = Board l' m'
  where
    push t l m = case M.lookup l m of
        Nothing -> error "pushed into Nothingness - this is a bug"
        Just Wall -> Nothing
        Just Empty -> Just $ M.insert l t m
        Just Box -> push Box (next l dir) $ M.insert l t m
        Just RB
            | dir == L || dir == R -> push RB (next l dir) $ M.insert l t m
            | otherwise -> do
                  m' <- push RB (next l dir) $ M.insert l t m
                  let ol = second (subtract 1) l
                  push LB (next ol dir) $ M.insert ol Empty m'
        Just LB
            | dir == L || dir == R -> push LB (next l dir) $ M.insert l t m
            | otherwise -> do
                  m' <- push LB (next l dir) $ M.insert l t m
                  let ol = second (+1) l
                  push RB (next ol dir) $ M.insert ol Empty m'
    (l', m') = case push Empty (next l0 dir) m0 of
        Nothing -> (l0, m0)
        Just m -> (next l0 dir, m)

next :: (Int, Int) -> Direction -> (Int, Int)
next (r, c) dir = case dir of
    U -> (r - 1, c)
    R -> (r, c + 1)
    D -> (r + 1, c)
    L -> (r, c - 1)


draw :: Board -> IO ()
draw (Board l m) = do
    let ((mr, mc), _) = M.findMax m
        ch r c = case M.lookup (r, c) m of
            Nothing -> '!'
            Just Empty -> '.'
            Just Box -> 'O'
            Just Wall -> '#'
            Just LB -> '['
            Just RB -> ']'
    for_ [0..mr] $ \r -> do
        for_ [0..mc] $ \c -> do
            putChar $ if l == (r, c) then '@' else ch r c
        putStrLn ""


run :: Board -> [Direction] -> Board
run = foldl' (flip step)

solve1 :: Board -> [Direction] -> Int
solve1 b ds = sum [ 100 * r + c | ((r, c), Box) <- M.toList m ]
  where
    Board _ m = run b ds


expand :: Board -> Board
expand (Board (rr, rc) m) = Board (rr, rc * 2) $
    M.fromList [ ((r, c * 2 + p), t')
               | ((r, c), t) <- M.toList m
               , (p, t') <- zip [0..] $ widen t
               ]
  where
    widen Wall = [Wall, Wall]
    widen Empty = [Empty, Empty]
    widen Box = [LB, RB]


solve2 :: Board -> [Direction] -> Int
solve2 b ds = sum [ 100 * r + c | ((r, c), LB) <- M.toList m ]
  where
    Board _ m = run (expand b) ds


main :: IO ()
main = do
    (board, dirs) <- parse <$> input 0
    print $ solve1 board dirs
    print $ solve2 board dirs
