#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}
{-# Language RecursiveDo #-}

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Data.Ord (Down(Down))
import Data.List (sortOn, tails)
import Control.Monad (guard, when)

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Data.STRef

type Coord = (Int, Int, Int)

parse :: String -> [Coord]
parse s = case readP_to_S full s of
            [(x, "")] -> x
            x -> error $ "Parse error: " ++ (show x)
  where
    full = endBy coord (char '\n') <* eof
    coord = (,,) <$> num <* char ',' <*> num <* char ',' <*> num
    num = read <$> munch1 isDigit


d2 :: Coord -> Coord -> Int
d2 (x1, y1, z1) (x2, y2, z2) = dx * dx + dy * dy + dz * dz
  where
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2

-- Reports a trace of running Kruskal's algorithm on a complete graph
--
-- Takes a distance function and a list of nodes
--
-- Returns a list of steps taken:
--
-- 1. The pair of nodes considered
-- 2. Whether the pair of nodes connected separate trees
-- 3. The sizes of the trees in the disjoint forest after ensuring
--    the considered nodes are in the same tree, in descending order
--
-- The output list is terminated when the spanning tree is complete.
--
-- Space invariants:
--
-- - No pattern of consumption of the result list will cause nested
--   unevaluted expressions to be built up.
kruskal :: (Ord a, Ord b) => (a -> a -> b) -> [a] -> [((a, a), Bool, [Int])]
kruskal dist cs = runST $ do
    uf <- newUnionForest
    keyed <- mapM (\x -> (,) x <$> newKey uf) cs
    let dist' ((c1, _), (c2, _)) = dist c1 c2
        pairs = sortOn dist' [ (c1, c2) | (c1:xs) <- tails keyed, c2 <- xs ]

        go 1 _ = pure []
        go _ [] = error "kruskal: Somehow ran out of edges without fully connecting the graph. Given that this takes a complete graph as input, this should be impossible"
        go i (((c1, k1), (c2, k2)):xs) = do
            altered <- union k1 k2 uf
            sizes <- forestSizes uf
            let i' = if altered then i - 1 else i
            rest <- unsafeInterleaveST $ go i' xs
            pure $ ((c1, c2), altered, sortOn Down sizes) : rest

    go (length cs) pairs

solve :: [Coord] -> (Int, Int)
solve cs = (p1, p2)
  where
    ((_, _, circuits):rest) = drop 999 $ kruskal d2 cs
    p1 = product . take 3 $ circuits
    p2 = x1 * x2
      where
        (((x1, _, _), (x2, _, _)), True,  _) = last rest

main :: IO ()
main = do
    (part1, part2) <- solve . parse <$> readFile "input/08.txt"
    print part1
    print part2








-- STRef-based Union-Find, tracking roots with a doubly-linked list.
-- Not actually very fast, with all the overhead, despite everything
-- being optimal complexity.

newtype UnionForest s = UF (STRef s (Maybe (RootList s)))
newtype Key s = K (STRef s (Either (Key s) (RootList s)))

data RootList s = RLNode
    { rlCount :: (STRef s Int)
    , rlPrev, rlNext :: (STRef s (RootList s))
    }

newUnionForest :: ST s (UnionForest s)
newUnionForest = UF <$> newSTRef Nothing

newKey :: UnionForest s -> ST s (Key s)
newKey (UF ufr) = do
    c <- newSTRef 1

    end <- readSTRef ufr
    node <- case end of
        Nothing -> do
            rec let rl = RLNode c p n
                p <- newSTRef rl
                n <- newSTRef rl
            writeSTRef ufr (Just rl)
            pure rl
        Just after -> do
            before <- readSTRef (rlPrev after)
            p <- newSTRef before
            n <- newSTRef after
            let rl = RLNode c p n
            writeSTRef (rlNext before) rl
            writeSTRef (rlPrev after) rl
            pure rl
    K <$> newSTRef (Right node)

union :: Key s -> Key s -> UnionForest s -> ST s Bool
union k1 k2 (UF ufRef) = do
    let find (K currentRef) = do
            contents <- readSTRef currentRef
            case contents of
                Left nextK@(K nextRef) -> do
                    res@(finalRef, _) <- find nextK
                    -- path compression
                    when (nextRef /= finalRef) $ writeSTRef currentRef (Left $ K finalRef)
                    pure res
                Right node -> pure (currentRef, node)
    (ref1, node1) <- find k1
    (ref2, node2) <- find k2
    case ref1 == ref2 of
        True -> pure False
        False -> do
            -- union by rank
            count1 <- readSTRef (rlCount node1)
            count2 <- readSTRef (rlCount node2)
            let (smallerKeyRef, largerKey, smallerNode, largerNode)
                    | count1 < count2 = (ref1, k2, node1, node2)
                    | otherwise = (ref2, k1, node2, node1)
            writeSTRef smallerKeyRef $ Left largerKey
            writeSTRef (rlCount largerNode) $! count1 + count2

            -- splice the smaller node out of the root list
            pNode <- readSTRef (rlPrev smallerNode)
            nNode <- readSTRef (rlNext smallerNode)
            writeSTRef (rlNext pNode) nNode
            writeSTRef (rlPrev nNode) pNode

            -- ensure the removed node isn't the global head of the root list
            writeSTRef ufRef $ Just largerNode

            pure True

forestSizes :: UnionForest s -> ST s [Int]
forestSizes (UF ref) = do
    contents <- readSTRef ref
    case contents of
        Nothing -> pure []
        Just start -> do
            c <- readSTRef (rlCount start)
            end <- rlNext <$> readSTRef (rlPrev start)
            let go r | r == end = pure []
                     | otherwise = do
                           n <- readSTRef r
                           i <- readSTRef (rlCount n)
                           more <- go (rlNext n)
                           pure $ i : more
            rest <- go (rlNext start)
            pure $ c : rest
