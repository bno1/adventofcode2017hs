module Main where

import Common
import Common.KnotHash
import Data.Char (isDigit)
import Text.Printf


main :: IO ()
main = do
    input_raw <- getLine
    let input1 = map read . wordsBy isDigit $ input_raw

    let result1 = khRoundW8 input1
    let result2 = khHashStr input_raw

    putStr "Solution 1: "
    print $ product $ take 2 (map fromIntegral result1 :: [Int])

    putStr "Solution 2: "
    putStrLn $ concatMap (printf "%02x") result2
