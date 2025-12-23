#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/02.txt"
         | otherwise = "example/02-" ++ show n ++ ".txt"

parse :: String -> [(Int, Int, Int)]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = many line <* eof
    line = (,,) <$> num <* x <*> num <* x <*> num <* nl
    x = char 'x'
    nl = char '\n'
    num = read <$> munch1 isDigit

solve1 :: [(Int, Int, Int)] -> Int
solve1 = sum . map wrapArea

wrapArea :: (Int, Int, Int) -> Int
wrapArea (x, y, z) = 2 * (s1 + s2 + s3) + minimum [s1, s2, s3]
  where
    s1 = x * y
    s2 = x * z
    s3 = y * z

solve2 :: [(Int, Int, Int)] -> Int
solve2 = sum . map ribbonLength

ribbonLength :: (Int, Int, Int) -> Int
ribbonLength (x, y, z) = minimum [p1, p2, p3] + bow
  where
    p1 = 2 * (x + y)
    p2 = 2 * (x + z)
    p3 = 2 * (y + z)

    bow = x * y * z

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
