#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Maybe


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "03"


decimal :: ReadP Int
decimal = read <$> munch1 (isDigit)

toPairs :: String -> [(Int, Int)]
toPairs s = case readP_to_S full s of
              [(x, "")] -> x
              x -> error $ "Parse error: " ++ (show x)
  where
    full = catMaybes <$> manyTill possimul eof
    mul = (,) <$ string "mul(" <*> decimal <* char ',' <*> decimal <* char ')'
    possimul = (Just <$> mul) <++ (Nothing <$ get)


toEnabledPairs :: String -> [(Int, Int)]
toEnabledPairs s =  case readP_to_S full s of
                      [(x, "")] -> x
                      x -> error $ "Parse error: " ++ (show x)
  where
    full = catMaybes <$> manyTill possimul eof
    mul = (,) <$ string "mul(" <*> decimal <* char ',' <*> decimal <* char ')'
    don't = string "don't()" <* manyTill get ((() <$ string "do()") +++ eof)
    possimul = (Just <$> mul) <++ (Nothing <$ don't) <++ (Nothing <$ get)


main :: IO ()
main = do
    inp <- input 0

    print $ sum . map (uncurry (*)) . toPairs $ inp
    print $ sum . map (uncurry (*)) . toEnabledPairs $ inp
