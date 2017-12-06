module Main where

import Control.Arrow ((***))
import Data.List (tails)


minMax :: Integral a => [a] -> a
minMax [] = 0
minMax l = uncurry (-) $ foldr1 (\(x, y) -> max x *** min y) $ zip l l

quotient :: Integral a => [a] -> a
quotient [] = 0
quotient l = head [
        q
        | (x:xs) <- tails l
        , y <- xs
        , let (q, r) = quotRem (max x y) (min x y)
        , r == 0
    ]

main :: IO ()
main = do
    matrix <- ((map (map read . words) . lines) <$> getContents) :: IO [[Int]]

    putStr "Solution 1: "
    print $ sum $ map minMax matrix

    putStr "Solution 2: "
    print $ sum $ map quotient matrix
