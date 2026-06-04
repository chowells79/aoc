#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isLower, isUpper)

import Data.List (nub)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/19.txt"
         | otherwise = "example/19-" ++ show n ++ ".txt"

parse :: String -> ([(String, String)], String)
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = (,) <$> replacements <*> molecule <* skipSpaces <* eof

    replacements = endBy pair (char '\n') <* char '\n'
    pair = (,) <$> molecule <* string " => " <*> molecule

    molecule = munch1 isAlpha



solve1 :: ([(String, String)], String) -> Int
solve1 = length . nub . uncurry substitutions


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp




-- Take a table of substitutions and an input. For each consecutive
-- subsequence of the input that matches an entry in the table,
-- perform the specified replacement exactly once.
substitutions :: Eq a => [([a], [a])] -> [a] -> [[a]]
substitutions rawTable xs =
    foldr (\(m, r) z -> subst m r xs z) [] rawTable



subst :: Eq a => [a] -> [a] -> [a] -> [[a]] -> [[a]]
subst [] _ = error "sub1 empty match, I'm too lazy to handle this correctly"
subst (mH:mT) replacement = start id
  where
    start _ [] = id
    start p (i:is)
        | mH == i = continue p mT is . start (p . (i :)) is
        | otherwise = start (p . (i :)) is

    continue p = go
      where
        go []     iis = ((p replacement ++ iis) :)
        go _      [] = id
        go (m:ms) (i:is)
            | m == i = go ms is
            | otherwise = id
