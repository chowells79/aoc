#!/usr/bin/env cabal
{- cabal:
build-depends: base, array
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Array.IArray

import Data.List (intercalate, isSuffixOf, sort)

import Data.Bits (xor, shift)

import Debug.Trace

import Data.Foldable
import Control.Monad

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "17"


type Program = Array Int Int
data Registers = R !Int !Int !Int deriving (Eq, Ord, Show)

data Machine = M !Int !Registers !Program deriving (Eq, Ord, Show)

parse :: String -> Machine
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = M 0 <$> registers <* char '\n' <*> program <* eof

    registers = R <$> register <*> register <*> register
    register = hnum <* char '\n'

    toProgram ls = listArray (0, length ls - 1) ls
    rawProgram = string "Program: " *> sepBy num (char ',') <* char '\n'
    program = toProgram <$> rawProgram

    hnum = num <++ (get *> hnum)
    num = read <$> munch1 isDigit


step :: Machine -> Maybe (Maybe Int, Machine)
step (M ip (R a b c) prog) = do
    opcode <- prog !? ip
    operand <- prog !? (ip + 1)
    case opcode of
        0 -> noOut $ M (ip + 2) (R a' b c) prog
          where
            a' = a `shift` negate (combo operand)
        1 -> noOut $ M (ip + 2) (R a (b `xor` operand) c) prog
        2 -> noOut $ M (ip + 2) (R a (combo operand `mod` 8) c) prog
        3 | a == 0 -> noOut $ M (ip + 2) (R a b c) prog
          | otherwise -> noOut $ M operand (R a b c) prog
        4 -> noOut $ M (ip + 2) (R a (b `xor` c) c) prog
        5 -> out (combo operand `mod` 8) $ M (ip + 2) (R a b c) prog
        6 -> noOut $ M (ip + 2) (R a b' c) prog
          where
            b' = a `shift` negate (combo operand)
        7 -> noOut $ M (ip + 2) (R a b c') prog
          where
            c' = a `shift` negate (combo operand)
        _ -> Nothing
  where
    combo x
        | x <= 3 = x
        | x == 4 = a
        | x == 5 = b
        | x == 6 = c
        | otherwise = error "invalid combo input"
    noOut x = pure (Nothing , x)
    out o x = pure (Just o, x)


run :: Machine -> [Int]
run m = case step m of
    Nothing -> []
    Just (Nothing, m') -> run m'
    Just (Just o, m') -> o : run m'

solve1 :: Machine -> String
solve1 = intercalate "," . map show . run

ex :: Machine -> Int -> [Int]
ex (M ip (R _ b c) prog) i = run $ M ip (R i b c) prog


explore :: Machine -> [Int]
explore m@(M _ _ prog) = go 0
  where
    target = toList prog

    go n = do
        d <- [(if n == 0 then 1 else 0) .. 7]
        let n' = n + d
            ran = ex m n'
        if ran == target
            then pure n'
            else do
                guard $ ran `isSuffixOf` target
                go $ n' * 8

main :: IO ()
main = do
    inp <- parse <$> input 0
    putStrLn $ solve1 inp

    print . head. sort $ explore inp
