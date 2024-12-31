#!/usr/bin/env cabal
{- cabal:
build-depends: base, linear
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Linear.Matrix
import Linear.V2

import Control.Monad

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
    button = (,) <$> findNum <*> findNum <* manyTill get (char '\n')
    prize = (,) <$> findNum <*> findNum <* manyTill get (char '\n')

    findNum = num <++ (get *> findNum)
    num = fromInteger . read <$> munch1 isDigit


algebra :: Problem -> Maybe (Rational, Rational)
algebra ((xa, ya), (xb, yb), (xf, yf))
    | det22 mat == 0 = Nothing
    | otherwise = Just (a, b)
  where
    V2 a b = inv22 mat !* V2 xf yf
    mat = V2 (V2 xa xb) (V2 ya yb)

solution :: Rational -> Rational -> Maybe Integer
solution a b
    | a < 0 || b < 0 = Nothing
    | denominator a > 1 || denominator b > 1 = Nothing
    | otherwise = Just $ 3 * numerator a + numerator b

solve :: [Problem] -> Integer
solve = sum . mapMaybe (uncurry solution <=< algebra)

relocate :: Problem -> Problem
relocate (a, b, (x, y)) = (a, b, (x + d, y + d))
  where
    d = 10000000000000

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve inp
    print . solve $ map relocate inp
