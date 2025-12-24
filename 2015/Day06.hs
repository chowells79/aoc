#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers >= 0.8
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/06.txt"
         | otherwise = "example/06-" ++ show n ++ ".txt"

data Step = Step Op Coord Coord deriving (Eq, Ord, Show)
data Op = On | Off | Toggle deriving (Eq, Ord, Show)
data Coord = Coord Int Int deriving (Eq, Ord, Show)

parse :: String -> [Step]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy line (char '\n') <* skipSpaces <* eof
    line = Step <$> op <*> coord <* string " through " <*> coord
    op = (On <$ string "turn on ") +++
         (Off <$ string "turn off ") +++
         (Toggle <$ string "toggle ")
    coord = Coord <$> num <* char ',' <*> num
    num = read <$> munch1 isDigit


step1 :: Set Coord -> Step -> Set Coord
step1 s (Step op (Coord x1 y1) (Coord x2 y2)) = combine s box
  where
    box = S.fromList [ Coord x y
                     | x <- [min x1 x2 .. max x1 x2]
                     , y <- [min y1 y2 .. max y1 y2]
                     ]
    combine = case op of
        On -> S.union
        Off -> S.difference
        Toggle -> S.symmetricDifference

solve1 :: [Step] -> Int
solve1 = S.size . foldl' step1 S.empty


step2 :: Map Coord Int -> Step -> Map Coord Int
step2 m (Step op (Coord x1 y1) (Coord x2 y2)) = final
  where
    final = case op of
        On -> M.unionWith (+) m box
          where
            box = M.fromList [ (Coord x y, 1)
                             | x <- [min x1 x2 .. max x1 x2]
                             , y <- [min y1 y2 .. max y1 y2]
                             ]
        Off -> M.filter (> 0) $ M.unionWith (+) m box
          where
            box = M.fromList [ (Coord x y, -1)
                             | x <- [min x1 x2 .. max x1 x2]
                             , y <- [min y1 y2 .. max y1 y2]
                             ]
        Toggle -> M.unionWith (+) m box
          where
            box = M.fromList [ (Coord x y, 2)
                             | x <- [min x1 x2 .. max x1 x2]
                             , y <- [min y1 y2 .. max y1 y2]
                             ]

solve2 :: [Step] -> Int
solve2 = sum . foldl' step2 M.empty

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
