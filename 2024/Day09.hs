#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.Char (isDigit)
import Data.Maybe

import Data.Foldable
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "09"

parse :: String -> [Int]
parse = map (read . pure) . filter isDigit

sparsify :: [Int] -> Seq (Maybe Int)
sparsify compact = Seq.fromList
    [ toId position
    | (position, count) <- zip [0..] compact
    , _ <- [1 .. count]
    ]

toId :: Int -> Maybe Int
toId p = case p `divMod` 2 of
           (i, 0) -> Just i
           _ -> Nothing

moveBlocks1 :: Seq (Maybe Int) -> [Int]
moveBlocks1 Empty = []
moveBlocks1 (s :|> Nothing) = moveBlocks1 s
moveBlocks1 (Just i :<| s) = i : moveBlocks1 s
moveBlocks1 (Nothing :<| (s :|> Just i)) = i : moveBlocks1 s

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

solve1 :: [Int] -> Int
solve1 = checksum . moveBlocks1 . sparsify


annotate :: [Int] -> Seq (Int, Maybe Int)
annotate compact = Seq.fromList
    [ (count, toId position)
    | (position, count) <- zip [0..] compact
    ]


moveBlocks2 :: Seq (Int, Maybe Int) -> Seq (Int, Maybe Int)
moveBlocks2 Empty = Empty
moveBlocks2 (p@(_, Just _) :<| ps) = p :<| moveBlocks2 ps
moveBlocks2 (ps :|> p@(_, Nothing)) = moveBlocks2 ps :|> p
moveBlocks2 (ps :|> p@(c, Just _)) =
    case insertFirstFit ps of
        Just ps' -> moveBlocks2 ps' :|> (c, Nothing)
        Nothing -> moveBlocks2 ps :|> p
  where
    insertFirstFit Empty = Nothing
    insertFirstFit (q@(_, Just _) :<| qs) = (q :<|) <$> insertFirstFit qs
    insertFirstFit (q@(c', Nothing) :<| qs)
        | c' < c = (q :<|) <$> insertFirstFit qs
        | c' == c = Just $ p :<| qs
        | otherwise = Just $ p :<| (c' - c, Nothing) :<| qs


sparseAnnotations :: Seq (Int, Maybe Int) -> [Int]
sparseAnnotations s = [ fromMaybe 0 mi | (n, mi) <- toList s, _ <- [1..n] ]

solve2 :: [Int] -> Int
solve2 = checksum . sparseAnnotations . moveBlocks2 . annotate


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp

    print $ solve2 inp
