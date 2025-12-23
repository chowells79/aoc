#!/usr/bin/env cabal
{- cabal:
build-depends: base, pureMD5, bytestring
-}

import Data.Char (isAlpha)

import Data.Digest.Pure.MD5 (md5)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/04.txt"
         | otherwise = "example/04-" ++ show n ++ ".txt"

parse :: String -> String
parse = filter isAlpha

solve1 :: String -> Int
solve1 s = snd . head . filter ((== "00000") . take 5 . fst) $ candidates
  where
    candidates = map (\n -> (show . md5 . B.fromStrict . B.pack $ s ++ show n, n)) [1..]

solve2 :: String -> Int
solve2 s = snd . head . filter ((== "000000") . take 6 . fst) $ candidates
  where
    candidates = map (\n -> (show . md5 . B.fromStrict . B.pack $ s ++ show n, n)) [1..]

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
