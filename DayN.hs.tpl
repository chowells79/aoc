#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "${id}"


main :: IO ()
main = do
    inp <- input 1
    print inp
