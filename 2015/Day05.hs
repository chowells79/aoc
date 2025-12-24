#!/usr/bin/env cabal
{- cabal:
build-depends: base, machines, containers
-}
{-# Language BangPatterns #-}

import Data.Machine.Mealy
import Data.Monoid

import qualified Data.Set as S

input :: Int -> IO String
input n = readFile name
  where
    name | n == 0 = "input/05.txt"
         | otherwise = "example/05-" ++ show n ++ ".txt"


final :: b -> Mealy a b -> [a] -> b
final b _ [] = b
final b m (x:xs) = uncurry final (runMealy m x) xs

latch :: b -> (b, Mealy a b)
latch b = (b, pure b)

stateMealy' :: b -> (b -> a -> b) -> Mealy a b
stateMealy' z f = go z
  where
    go s = Mealy $ \a -> let !s' = f s a in (s', go s')


count :: (a -> Bool) -> Mealy a Int
count p = stateMealy' 0 $ \i a -> if p a then i + 1 else i

twoInARow :: Eq a => Mealy a Bool
twoInARow = Mealy $ \c -> (False, go c)
  where
    go p = Mealy $ \c -> if p == c then latch True else (False, go c)

hasConsecutively :: Eq a => a -> a -> Mealy a Bool
hasConsecutively a b = start
  where
    start = Mealy $ \c -> (False, if c == a then finish else start)
    finish = Mealy $ \c -> if c == b then latch True else runMealy start c

isNice1 :: String -> Bool
isNice1 = final False . fmap getAll . foldMap (fmap All) $
    [ (3 <=) <$> count (`elem` "aeiou")
    , twoInARow
    , not . getAny <$> foldMap (fmap Any)
      [ hasConsecutively 'a' 'b'
      , hasConsecutively 'c' 'd'
      , hasConsecutively 'p' 'q'
      , hasConsecutively 'x' 'y'
      ]
    ]

solve1 :: [String] -> Int
solve1 = length . filter isNice1


gapPair :: Eq a => Mealy a Bool
gapPair = Mealy $ \a -> (False, two a)
  where
    two a = Mealy $ \b -> (False, go a b)
    go a b = Mealy $ \c -> if a == c then latch True else (False, go b c)

pairOfTwo :: Ord a => Mealy a Bool
pairOfTwo = Mealy $ \a -> (False, two a)
  where
    two a = Mealy $ \b -> (False, three a b)
    three a b = Mealy $ \c -> (False, go (S.singleton [a, b]) a b c)
    go s a b c = Mealy $ \d -> if S.member [c, d] s
                               then latch True
                               else (False, go (S.insert [b, c] s) b c d)

isNice2 :: String -> Bool
isNice2 = final False $ liftA2 (&&) pairOfTwo gapPair

solve2 :: [String] -> Int
solve2 = length . filter isNice2


main :: IO ()
main = do
    inp <- lines <$> input 0
    print $ solve1 inp
    print $ solve2 inp
