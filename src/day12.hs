{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Main where

import Common.Parsers
import Control.Monad.State
import Data.List

import qualified Data.IntMap as M
import qualified Data.IntSet as S
import qualified Text.Parsec as P


type Graph = M.IntMap [Int]

parseLine :: String -> Either String (Int, [Int])
parseLine = parseErrToStr . P.runParser p () "parseLine"
    where
        p = do
            n <- parseIntegral
            P.spaces
            _ <- P.string "<->"
            P.spaces
            ns <- P.sepBy parseIntegral (P.spaces >> P.char ',' >> P.spaces)
            return (n, ns)

updateGraph :: Graph -> (Int, [Int]) -> Graph
updateGraph m (n, ns) =
    M.unionWith ((nub .) . (++)) m  $ M.fromList $ (n, ns) : zip ns (repeat [n])

graphDFS :: Graph -> Int -> (Int -> State s ()) -> State s ()
graphDFS m s = forM_ (unfoldr step ([s], S.singleton s))
    where
        step (next, visited) = do
            (n, next') <- uncons next
            let ns = filter (`S.notMember` visited) $ M.findWithDefault [] n m
            let visited' = S.union visited (S.fromList ns)

            return (n, (ns ++ next', visited'))

connectedComponents :: Graph -> M.IntMap [Int]
connectedComponents = helper M.empty
    where
        helper :: M.IntMap [Int] -> Graph -> M.IntMap [Int]
        helper comps m
            | M.null m = comps
            | otherwise = helper (M.insert n ns comps) m'
            where
                (n, _) = M.findMin m
                ns = execState (graphDFS m n (\x -> modify' (x:))) []
                m' = foldl (flip M.delete) m ns

main :: IO ()
main = do
    input <- map (either error id . parseLine) . lines <$> getContents

    let graph = foldl updateGraph M.empty input
    let components = connectedComponents graph

    putStr "Solution 1: "
    print $ length <$> M.lookup 0 components

    putStr "Solution 2: "
    print $ length components
