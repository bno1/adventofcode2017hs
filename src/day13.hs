module Main where

import Common
import Control.Monad.Fix
import Data.Char (isDigit)


parseLine :: String -> (Int, Int)
parseLine s = let [l, r] = map read $ wordsBy isDigit s in (l, r)

-- checks if a packet delayed `t` picoseconds will be detected by the scanner
-- at layer `l` with range `r`
detected :: Int -> (Int, Int) -> Bool
detected t (l, r) = (l + t) `rem` ((r - 1) * 2) == 0

severity :: [(Int, Int)] -> Int
severity = sum . map (uncurry (*)) . filter (detected 0)

shortestDelay :: [(Int, Int)] -> Int
shortestDelay scanners =
    fix (\f t -> if any (detected t) scanners then f (t + 1) else t) 0

main :: IO ()
main = do
    input <- map parseLine . lines <$> getContents

    putStr "Solution 1: "
    print $ severity input

    putStr "Solution 2: "
    print $ shortestDelay input
