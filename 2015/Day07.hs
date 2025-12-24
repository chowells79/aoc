#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isDigit)

import Data.Word (Word16)

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)

import Data.Map (Map)
import qualified Data.Map as M

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/07.txt"
         | otherwise = "example/07-" ++ show n ++ ".txt"


data Wire = Wire Op String deriving (Eq, Ord, Show)
data Op
    = Const Val
    | And Val Val
    | Or Val Val
    | Not Val
    | LShift Val Int
    | RShift Val Int
    deriving (Eq, Ord, Show)
data Val = W Word16 | L String deriving (Eq, Ord, Show)

parse :: String -> [Wire]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = sepBy wire (char '\n') <* skipSpaces <* eof
    wire = Wire <$> op <* string " -> " <*> label
    op = constO +++ andO +++ orO +++ notO +++ leftO +++ rightO
    constO = Const <$> val
    andO = And <$> val <* string " AND " <*> val
    orO = Or <$> val <* string " OR " <*> val
    notO = Not <$ string "NOT " <*> val
    leftO = LShift <$> val <* string " LSHIFT " <*> num
    rightO = RShift <$> val <* string " RSHIFT " <*> num

    val = (W <$> num) +++ (L <$> label)
    label = munch1 isAlpha
    num :: Read a => ReadP a
    num = read <$> munch1 isDigit

connect :: [Wire] -> Map String Word16
connect wires = circuit
  where
    circuit = M.fromList [ (label, calc op) | Wire op label <- wires ]

    calc (Const c) = val c
    calc (And x y) = val x .&. val y
    calc (Or x y) = val x .|. val y
    calc (Not x) = complement $ val x
    calc (LShift x n) = shiftL (val x) n
    calc (RShift x n) = shiftR (val x) n

    val (W w) = w
    val (L l) = circuit M.! l

solve1 :: [Wire] -> Word16
solve1 wires = connect wires M.! "a"

solve2 :: [Wire] -> Word16
solve2 wires = connect newWires M.! "a"
  where
    newB = connect wires M.! "a"
    newWires = reverse $ Wire (Const (W newB)) "b" : wires

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print $ solve2 inp
