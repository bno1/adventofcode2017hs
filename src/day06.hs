module Main where

import Control.Monad (msum)
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU


step :: (VG.Vector v a, Integral a) => v a -> v a
step v = VG.generate vlen gen
    where
        vlen = VG.length v

        -- find the biggest bank
        fold_func (i', x') i x = if x > x' then (i, x) else (i', x')
        (mi, mx) = VG.ifoldl fold_func (0, v VG.! 0) v

        -- the number of loops through the vector when distributing the blocks
        (q, r) = fromIntegral mx `quotRem` vlen

        -- compute the new number of blocks for a bank
        -- uses q and r to determine how many times the distribution function
        -- will pass over the bank i
        --
        -- computations are relative to the longest bank:
        -- rel_i = 0 for the longest bank, 1 for the one after it, etc
        gen i = let rel_i = (vlen - mi +  i) `rem` vlen in
            (if rel_i == 0 then 0 else v VG.! i) +
            fromIntegral q +
            (if rel_i > 0 && rel_i <= r then 1 else 0)

findCycle :: (VG.Vector v a, Integral a, Eq (v a)) => v a -> (Int, Int)
findCycle vi =  fromJust $ msum $ map check $ iterate step' [vi]
    where
        step' [] = [vi]
        step' lst@(v:_) = step v : lst

        check [] = Nothing
        check (v:vs) = do
            i <- elemIndex v vs
            return (length vs, i + 1)

main :: IO ()
main = do
    input <- (VU.fromList . map read . words <$> getLine) :: IO (VU.Vector Int)

    let (s1, s2) = findCycle input

    putStr "Solution 1: "
    print s1

    putStr "Solution 2: "
    print s2
