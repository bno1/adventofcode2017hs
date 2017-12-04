module Main where

import Data.List
import qualified Data.Set as S


hasDuplicates :: (Traversable t, Ord a) => t a -> Bool
hasDuplicates = or . snd . mapAccumL f S.empty
    where f s x = let s' = S.insert x s in (s', S.size s' == S.size s)

main :: IO ()
main = do
    input <- lines <$> getContents
    let count = length . filter id

    putStr "Solution 1: "
    print $ count $ map (not . hasDuplicates . words) input

    putStr "Solution 2: "
    print $ count $ map (not . hasDuplicates . map sort . words) input
