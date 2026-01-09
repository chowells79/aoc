#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isLower, isUpper)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/19.txt"
         | otherwise = "example/19-" ++ show n ++ ".txt"

parse :: String -> (Map String [[String]], [String])
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = (,) <$> replacements <*> molecule <* skipSpaces <* eof
    nl = char '\n'

    replacements = M.fromListWith (++) <$> endBy pair nl <* nl
    pair = (,) <$> munch1 isAlpha <* string " => " <*> (pure <$> molecule)

    molecule = many1 $ liftA2 (:) (satisfy isUpper) (munch isLower)

step :: Map String [[String]] -> [String] -> [[String]]
step r m = S.toList (S.delete m substs)
  where
    substs = S.fromList $ go m
    go [] = [[]]
    go (a:as) = case M.lookup a r of
        Nothing -> map (a :) $ go as
        Just ss -> map (++ as) ss ++ map (a:) (go as)


solve1 :: (Map String [[String]], [String]) -> Int
solve1 (r, m) = length $ step r m


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
