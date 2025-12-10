#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, MIP, data-default, scientific
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Traversable (forM)
import Data.List (transpose)
import Data.Bits (xor, (.&.))

import Data.Set (Set)
import qualified Data.Set as S



import Data.String (fromString)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Default (def)
import Data.Scientific (Scientific, toBoundedInteger)

import qualified Numeric.Optimization.MIP as MIP
import Numeric.Optimization.MIP.Solver as MIP
import Numeric.Optimization.MIP ((.==.))



input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/10.txt"
         | otherwise = "example/10-" ++ show n ++ ".txt"

data Machine = M Int [Int] [Int] deriving (Eq, Ord, Show)

parse :: String -> [Machine]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = endBy line (char '\n') <* eof
    line = M <$> lights <* char ' ' <*> buttons <*> joltage
    lights = char '[' *> binNum <* char ']'
    binNum = fromBin <$> many ((0 <$ char '.') +++ (1 <$ char '#'))
    fromBin = sum . zipWith (*) (iterate (*2) 1)
    buttons = endBy button (char ' ')
    button = sum . map (2^)  <$ char '(' <*> sepBy num (char ',') <* char ')'
    joltage = char '{' *> sepBy num (char ',') <* char '}'
    num = read <$> munch1 isDigit

solve1 :: [Machine] -> Int
solve1 = sum . map presses
  where
    presses (M target vectors _) = go 0 S.empty (S.singleton 0)
      where
        go g seen current
            | S.member target current = g
            | otherwise = go g' seen' current'
          where
            g' = g + 1
            seen' = S.union seen current
            current' = S.difference
                       (S.fromList [ c `xor` b
                                   | c <- S.toList current
                                   , b <- vectors
                                   ])
                       seen'


machineProblem :: Machine -> MIP.Problem Scientific
machineProblem (M _ vectors targets) =
    def
    { MIP.objectiveFunction = obFunc
    , MIP.constraints = constrs
    , MIP.varDomains = domains
    }
  where
    obFunc =
        def
        { MIP.objDir = MIP.OptMin
        , MIP.objExpr = sum exprs
        }

    constrs =
        [ sum (buttons bitStr) .==. fromIntegral joltage
        | (joltage, bitStr) <- zip targets $ transpose bitMatrix
        ]

    domains =
        M.fromList
        [ (v, (MIP.IntegerVariable, (0, fromIntegral maxPresses)))
        | v <- vars
        ]

    varCount = length targets
    maxPresses = sum targets

    vars = [ fromString $ "v" ++ show n | n <- [1 .. length vectors] ]
    exprs = map MIP.varExpr vars

    bitMatrix = map (take varCount . bits) vectors
    bits b = b .&. 1 : bits (b `div` 2)

    buttons = map fst . filter ((== 1) . snd) . zip exprs


solve2 :: [Machine] -> IO Int
solve2 ms = do
    presses <- forM ms $ \m -> do
        let problem = machineProblem m
        sol <- MIP.solve MIP.glpsol def problem

        case (MIP.solStatus sol) of
            MIP.StatusOptimal -> let Just val = MIP.solObjectiveValue sol
                                     Just res = toBoundedInteger val
                                 in pure res
            _ -> fail $ "No optimal solution found for " ++ show m

    pure $ sum presses

main :: IO ()
main = do
    inp <- parse <$> input 0
    print $ solve1 inp
    print =<< solve2 inp
