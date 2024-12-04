#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec
-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Void
import Data.Maybe


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "03"


toPairs :: String -> [(Int, Int)]
toPairs s = case parse pairs "" s of
    Left errs -> error $ errorBundlePretty errs
    Right xs -> catMaybes xs
  where
    pairs :: Parsec Void String [Maybe (Int, Int)]
    pairs = manyTill possimul eof
    mul = (,) <$ string "mul(" <*> decimal <* char ',' <*> decimal <* char ')'
    possimul = Just <$> try mul <|> Nothing <$ anySingle


toEnabledPairs :: String -> [(Int, Int)]
toEnabledPairs s = case parse pairs "" s of
    Left errs -> error $ errorBundlePretty errs
    Right xs -> catMaybes xs
  where
    pairs :: Parsec Void String [Maybe (Int, Int)]
    pairs = manyTill possimul eof
    mul = (,) <$ string "mul(" <*> decimal <* char ',' <*> decimal <* char ')'
    don't = string "don't()" <* manyTill anySingle (() <$ string "do()" <|> eof)
    possimul = Just <$> try mul <|>
               Nothing <$ try don't <|>
               Nothing <$ anySingle


main :: IO ()
main = do
    inp <- input 0

    print $ sum . map (uncurry (*)) . toPairs $ inp
    print $ sum . map (uncurry (*)) . toEnabledPairs $ inp
