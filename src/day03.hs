module Main where

import Control.Arrow ((***), first, second)
import Control.Monad.State
import Data.Maybe (fromJust)

import qualified Data.Sequence as S


-- TODO document better

seqLookup :: Int -> S.Seq a -> Maybe a
seqLookup i s = if S.length s <= i then Nothing else Just $ S.index s i

spiralToCart :: (Integral a, Show a) => a -> (a, a)
spiralToCart 1 = (0, 0)
spiralToCart n = case q of
        0 -> (a, b)
        1 -> (-b, a)
        2 -> (-a, -b)
        3 -> (b, -a)
        x -> error ("Unexpected quotient " ++ show x)
    where lvl = (1+) . ceiling $ (sqrt (fromIntegral n :: Float) - 3.0) / 2.0
          base = 2 + 4 * lvl * (lvl - 1)
          (q, r) = quotRem (n - base) (2 * lvl)
          a = lvl
          b = 1 - lvl + r

makeEdge :: (Integral a) => Int -> Int -> State (S.Seq a, a) ()
makeEdge lvl e
    | e < 0 || e > 3 = undefined
    | otherwise = forM_ [0..len] helper1 >> helper2
    where
        -- Length of the new spiral level
        len = 2 * (lvl + 1) - 1

        -- Index of the first element on spiral edge
        base = e * 2 * lvl

        -- First element neighbouring new element i
        r_start i = base + max i 1 - 1

        -- Last element neighbouring new element i
        r_end i = base + min (i + 1) (2 * lvl)

        -- S.Seq of elements neighbouring new element i
        range i = S.take (r_end i - r_start i + 1) . S.drop (r_start i)

        -- Compute element i and update accumulator
        helper1 :: (Integral a) => Int -> State (S.Seq a, a) ()
        helper1 i = do
            (s, acc) <- get
            let r = sum $ range i s
            put (s S.|> (r + acc), r + acc)

        -- Adds the element around the corner
        helper2 :: (Integral a) => State (S.Seq a, a) ()
        helper2 = do
            (s, _) <- get
            let v = S.index s (S.length s - 2)
            modify' $ second (+v)

spiralGreaterThan :: (Integral a) => a -> a
spiralGreaterThan n = fromJust $ msum $ map (hasGreater . fst) spirals
    where
        -- Returns the first element greater than n or Nothing
        hasGreater = seqLookup 0 . S.dropWhileL (<=n)

        -- Constructs the next level of the spiral with state monad
        step :: (Integral a) => Int -> State (S.Seq a, a) ()
        step lvl = do
            -- Prepend the last element
            modify' $ first $ \s -> S.index s (S.length s - 1) S.<| s

            -- Construct edges
            forM_ [0..3] (makeEdge lvl)

            -- Remove initial level
            modify' $ first $ S.drop (1 + max 1 (8 * lvl))

            -- Fix up bottom right corner
            v <- gets (flip S.index 0 . fst)
            modify' $ first $ \s -> S.adjust (+v) (S.length s - 2) s
            modify' $ first $ \s -> S.adjust (+(2*v)) (S.length s - 1) s

        -- Constructs the next level of the spiral
        nextSpiral (s, lvl) = (fst $ execState (step lvl) (s, 0), lvl + 1)

        spirals = iterate nextSpiral (S.singleton 1, 0)

mhNorm :: (Num a) => (a, a) -> a
mhNorm = uncurry (+) . (abs *** abs)

main :: IO ()
main = do
    input <- (read <$> getLine) :: IO Int

    putStr "Solution 1: "
    print $ mhNorm $ spiralToCart input

    putStr "Solution 2: "
    print $ spiralGreaterThan input
