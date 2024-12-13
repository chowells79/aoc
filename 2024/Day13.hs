#!/usr/bin/env cabal
{- cabal:
build-depends: base, linear
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Linear.Matrix
import Linear.V2

import Data.Maybe
import Data.Ratio

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "13"

type Problem =
    ((Rational, Rational), (Rational, Rational), (Rational, Rational))

parse :: String -> [Problem]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy1 machine (char '\n') <* eof

    machine = (,,) <$> button <*> button <*> prize
    button = (,) <$> hidden <*> hidden <* manyTill get (char '\n')
    prize = (,) <$> hidden <*> hidden <* manyTill get (char '\n')

    hidden = num <++ (get *> hidden)
    num = fromInteger . read <$> munch1 isDigit


algebra :: Problem -> Rational
algebra ((xa, ya), (xb, yb), (xf, yf)) = 3 * a + b
  where
    V2 a b = inv !* V2 xf yf
    inv = inv22 $ V2 (V2 xa xb) (V2 ya yb)

solution :: Rational -> Maybe Integer
solution x
    | denominator x == 1 = Just $ numerator x
    | otherwise = Nothing

solve :: [Problem] -> Integer
solve = sum . mapMaybe (solution . algebra)

relocate :: Problem -> Problem
relocate (a, b, (x, y)) = (a, b, (10000000000000 + x, 10000000000000 + y))


main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve inp
    print . solve $ map relocate inp



