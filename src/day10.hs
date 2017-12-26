module Main where

import Common
import Common.KnotHash
import Data.Char (isDigit, ord)
import Text.Printf


main :: IO ()
main = do
    input_raw <- getLine
    let input1 = map read . wordsBy isDigit $ input_raw
    let input2 = map ord input_raw ++ [17, 31, 73, 47, 23]

    let result1 = khRoundW8 input1
    let result2 = khHashW8 input2

    putStr "Solution 1: "
    print $ product $ take 2 (map fromIntegral result1 :: [Int])

    putStr "Solution 2: "
    putStrLn $ concatMap (printf "%02x") result2
