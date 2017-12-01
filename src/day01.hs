module Main where

import Data.Char (digitToInt)


solveCaptcha1 :: (Num a, Eq a) => [a] -> a
solveCaptcha1 str = helper 0 extended
    where extended = take (length str + 1) $ cycle str
          helper n (x:y:xs) = helper (if x == y then n + x else n) $ y:xs
          helper n _ = n

solveCaptcha2 :: (Integral a) => [a] -> a
solveCaptcha2 str = helper 0 str (h2 ++ h1)
    where (h1, h2) = splitAt (length str `div` 2) str
          helper n (x:xs) (y:ys) = helper (if x == y then n + x else n) xs ys
          helper n _ _ = n

main :: IO ()
main = do
    inputRaw <- getLine
    let input = map digitToInt inputRaw

    putStr "Solution 1: "
    print $ solveCaptcha1 input

    putStr "Solution 2: "
    print $ solveCaptcha2 input
