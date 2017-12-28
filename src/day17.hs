{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (foldl')


insertAfter :: Int -> [a] -> a -> [a]
insertAfter 0 xs y = y:xs
insertAfter _ [] y = [y]
insertAfter n (x:xs) y = x : insertAfter (n - 1) xs y

step :: Int -> (Int, [a]) -> a -> (Int, [a])
step skip (start, xs) a = (i + 1, insertAfter (i + 1) xs a)
    where
        i = (start + skip) `rem` length xs

findAfterZero :: Int -> Int -> Int
findAfterZero skip count = snd $ foldl' helper (0, 0) [1..count]
    where
        helper (!start, !val) i = (j + 1, if j == 0 then i else val)
            where j = (start + skip) `rem` i

main :: IO ()
main = do
    input <- read <$> getLine

    let (i, xs) = foldl (step input) (0, [0]) [1..2017] :: (Int, [Int])

    putStr "Solution 1: "
    print $ xs !! (i + 1)

    putStr "Solution 2: "
    print $ findAfterZero input 50000000
