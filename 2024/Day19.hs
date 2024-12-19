#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List

import Data.Map (Map)
import qualified Data.Map.Lazy as M


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


arrangements :: [String] -> String -> Int
arrangements towels pat = results M.! pat
  where
    results = M.fromList $ [ (p, arrs p) | p <- tails pat ]
    arrs p = sum $ do
        t <- towels
        case stripPrefix t p of
            Nothing -> [ 0 ]
            Just [] -> [ 1 ]
            Just xs -> [ results M.! xs ]


solve1 :: [String] -> [String] -> Int
solve1 towels = length . filter ((/= 0) . arrangements towels)

solve2 :: [String] -> [String] -> Int
solve2 towels = sum . map (arrangements towels)


main :: IO ()
main = do
    (ts, ps) <- parse <$> input 0
    print $ solve1 ts ps
    print $ solve2 ts ps
