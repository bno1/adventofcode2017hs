module Main where

import Common
import Data.Char (isLetter)
import Data.Maybe (fromMaybe)


data Dir
    = N
    | NW
    | NE
    | SE
    | SW
    | S
    deriving (Show, Eq)

data Pos = Pos Int Int Int
    deriving (Show, Eq)

-- Uses cube coordinates
step :: Pos -> Dir -> Pos
step (Pos x y z) N  = Pos x (y + 1) (z - 1)
step (Pos x y z) NW = Pos (x + 1) y (z - 1)
step (Pos x y z) NE = Pos (x - 1) (y + 1) z
step (Pos x y z) SE = Pos (x - 1) y (z + 1)
step (Pos x y z) SW = Pos (x + 1) (y - 1) z
step (Pos x y z) S  = Pos x (y - 1) (z + 1)

dist :: Pos -> Int
dist (Pos x y z) = maximum $ map abs [x, y, z]

parseDir :: String -> Dir
parseDir str = fromMaybe err $ lookup str dirs
    where
        err = error $ "Unknown direction " ++ str
        dirs = zip ["n", "ne", "nw", "se", "sw", "s"]
                   [ N,   NE,   NW,   SE,   SW,   S ]

main :: IO ()
main = do
    input <- map parseDir . wordsBy isLetter <$> getLine

    let steps = scanl step (Pos 0 0 0) input

    putStr "Solution 1: "
    print $ dist $ last steps

    putStr "Solution 2: "
    print $ maximum $ map dist steps
