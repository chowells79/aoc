#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.Bifoldable (bitraverse_)
import Data.List (stripPrefix, tails)
import Text.ParserCombinators.ReadP

import Data.Map (Map)
import qualified Data.Map as M


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "19"

parse :: String -> ([String], [String])
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = (,) <$> towels <*> patterns <* eof

    towels = sepBy1 (many1 color) (string ", ") <* string "\n\n"
    patterns = endBy1 (many1 color) (char '\n')

    color = choice $ map char "wubrg"


solve :: [String] -> [String] -> (Int, Int)
solve towels patterns = (sum $ map (min 1) counts, sum counts)
  where
    counts = map (results M.!) patterns
    results = M.fromList $ [ (p, arrs p) | pat <- patterns, p <- tails pat ]
    arrs p = sum $ do
        t <- towels
        pure $ case stripPrefix t p of
            Nothing -> 0
            Just [] -> 1
            Just xs -> results M.! xs

main :: IO ()
main = do
    (ts, ps) <- parse <$> input 0
    bitraverse_ print print $ solve ts ps
