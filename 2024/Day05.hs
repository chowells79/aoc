#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.List
import Data.Maybe

import qualified Data.Set as S


input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/" ++ ident ++ ".txt"
         | otherwise = "example/" ++ ident ++ "-" ++ show n ++ ".txt"
    ident = "05"


parse :: String -> ([(Int, Int)], [[Int]])
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = (,) <$> rules <* char '\n' <*> updates <* eof
    rules = endBy1 rule (char '\n')
    rule = (,) <$> num <* char '|' <*> num
    num = read <$> many1 (satisfy isDigit)
    updates = endBy1 update (char '\n')
    update = sepBy1 num (char ',')


middle :: [a] -> a
middle xs = head (drop (length xs `div` 2) xs)


-- A less-than function (<) that might not know how to compare two
-- particular values
type PO a = a -> a -> Maybe Bool

makePO :: Ord a => [(a, a)] -> PO a
makePO xs = lt
  where
    lt x y | S.member (x, y) lts = Just True
           | S.member (y, x) lts = Just False
           | otherwise = Nothing
    lts = S.fromList xs

ordered :: PO Int -> [Int] -> Bool
ordered lt xs = and
    [ all inOrder $ ys
    | (y:ys) <- tails xs
    , let inOrder = fromMaybe True . lt y
    ]

-- only terminates if there is an ordered permutation of the input
-- according to the partial order
bubbleSortPO :: PO Int -> [Int] -> [Int]
bubbleSortPO lt xs = head . dropWhile (not . ordered lt) . iterate bubble $ xs
  where
    bubble (x:y:zs)
        | Just True <- lt y x = y : bubble (x:zs)
        | otherwise = x : bubble (y:zs)
    bubble zs = zs


solve1 :: PO Int -> [[Int]] -> Int
solve1 lt = sum . map middle . filter (ordered lt)


solve2 :: PO Int -> [[Int]] -> Int
solve2 lt updates = sum . map middle $ corrected
  where
    corrected = map (bubbleSortPO lt) incorrect
    incorrect = filter (not . ordered lt) updates


main :: IO ()
main = do
    (pairs, updates) <- parse <$> input 0
    let lt = makePO pairs
    print $ solve1 lt updates
    print $ solve2 lt updates
