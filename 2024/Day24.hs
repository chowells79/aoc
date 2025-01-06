#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char

import Data.Map (Map)
import qualified Data.Map.Lazy as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Bits
import Data.Foldable

import Text.Printf

import Debug.Trace


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "24"


type Label = String
data Op = AND | OR | XOR deriving (Eq, Ord, Show)
type Input = (Label, Int)
type Gate = (Label, Op, Label, Label)
type Circuit = ([Input], [Gate])

parse :: String -> Circuit
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full :: ReadP Circuit
    full = (,) <$> inputs <* char '\n' <*> gates <* eof

    inputs :: ReadP [Input]
    inputs = endBy1 input (char '\n')
    input = (,) <$> label <* string ": " <*> num

    gates :: ReadP [Gate]
    gates = endBy1 gate (char '\n')
    gate = (,,,) <$> label <* w <*> op <* w <*> label <* string " -> " <*> label

    op = choice [ AND <$ string "AND", OR <$ string "OR", XOR <$ string "XOR" ]
    w = skipSpaces
    label = munch1 isAlphaNum
    num = read <$> munch1 isDigit


wireUp :: Circuit -> Map Label (Int, Set Label)
wireUp (labels, gates) = result
  where
    result = M.fromList $ map label labels ++ map gate gates
    label (l, i) = (l, (i, S.singleton l))
    gate (s1, op, s2, t) = (t, binop op (result M.! s1) (result M.! s2))
      where
        binop XOR (i, s1) (j, s2) = (i `xor` j, S.insert t $ S.union s1 s2)
        binop AND (i, s1) (j, s2) = (i .&. j, S.insert t $ S.union s1 s2)
        binop OR (i, s1) (j, s2) = (i .|. j, S.insert t $ S.union s1 s2)


solve2 :: Circuit -> Int
solve2 = toInt . output . wireUp

toInt :: [(Int, a)] -> Int
toInt = foldr' (\a b -> 2 * b + a) 0 . map fst

output :: Map Label (Int, Set Label) -> [(Int, Set Label)]
output = toList . snd . M.split "z"

add :: Circuit -> [Int] -> [Int] -> Map Label (Int, Set Label)
add (_, gates) is js = wireUp (xs ++ ys, gates)
  where
    xs = [ (printf "x%02d" x, min 1 i) | (x, i) <- zip [0 :: Int ..] is ]
    ys = [ (printf "y%02d" y, min 1 j) | (y, j) <- zip [0 :: Int ..] js ]

addZ :: Int -> Circuit -> Int -> Int -> Map Label (Int, Set Label)
addZ n c i j = add c (take n $ go i) (take n $ go j)
  where
    go x = case x `quotRem` 2 of (q, r) -> r : go q


runner :: [(Int, Set Label)] -> [(Int, Set Label)]
runner = go 0 S.empty . zip [0..]
    where
      go _ _ [] = []
      go r seen ((i, (x, g)):xs) = (r', s' S.\\ seen) : go r' s' xs
        where
          r' = r + x * 2 ^ i
          s' = S.union seen g


explore :: Int -> Circuit -> ()
explore n c = go 0
  where
    go = undefined



main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve2 inp

    print $ explore 45 inp
