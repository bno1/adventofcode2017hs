{-# LANGUAGE RankNTypes, LambdaCase #-}

module Main where

import Common
import Common.Parsers
import Control.Monad (forM_, liftM2)
import Control.Monad.ST
import Data.Char (ord)
import Data.List (elemIndex)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Text.Parsec as P


data Move
    = Spin Int
    | Exchange Int Int
    | Partner Int Int
    deriving (Eq, Show)

-- Dance per labels
--     per = permutation of positions created by Spin and Exchange moves
--     labels = label -> position permutation created by Partner moves
data Dance = Dance !(VU.Vector Int) !(VU.Vector Int)
    deriving (Eq)

identityPerm :: Int -> ST s (VUM.STVector s Int)
identityPerm n = do
    v <- VUM.new n
    forM_ [0..n - 1] $ \i -> VUM.write v i i
    return v

makeDance :: Int -> [Move] -> Dance
makeDance len moves = runST $ do
    per <- identityPerm len
    labels <- identityPerm len
    temp <- VUM.new len

    forM_ moves $ \case
        (Spin n) -> do
            VUM.copy temp per
            forM_ [0..len - 1] $ \i ->
                VUM.read temp ((i + len - n) `rem` len) >>= VUM.write per i
        (Exchange i j) -> VUM.swap per i j
        (Partner a b) -> VUM.swap labels a b

    liftM2 Dance (VU.freeze per) (VU.freeze labels)

printDance :: Dance -> String
printDance (Dance per labels) =
        map (chars !!) . VU.toList $ VU.backpermute labels' per
    where
        len = VU.length per
        chars = "abcdefghijklmnopqrstuvwxyz"

        -- turn label -> position into position -> label
        labels' = VU.replicate len 0 VU.// zip (VU.toList labels) [0..len]

-- Compose two dances by composing their permutations
composeDance :: Dance -> Dance -> Dance
composeDance (Dance per1 labels1) (Dance per2 labels2) =
    Dance (VU.backpermute per1 per2) (VU.backpermute labels1 labels2)

repeatDanceN :: Int -> Dance -> Dance
repeatDanceN n d = foldl (\d' _ -> composeDance d d') d [0..n - 2]

-- Find the cycle and returns (cycle start, cycle length)
findDanceCycle :: Dance -> (Int, Int)
findDanceCycle dance = helper dance []
    where
        helper d ds = case d `elemIndex` ds of
            Just i -> (length ds - i - 1, i + 1)
            Nothing -> helper (composeDance dance d) (d:ds)

-- Repeats a dance using its cycle
repeatDanceNLarge :: Int -> Dance -> Dance
repeatDanceNLarge n d = repeatDanceN cnt d
    where
        (start, len) = findDanceCycle d
        cnt = min n ((n - start) `rem` len)

parseMove :: String -> Either String Move
parseMove = parseErrToStr . P.parse p "parseMove"
    where
        p = do
            let parseProg = (\c -> ord c - ord 'a') <$> P.lower
            let pspin = P.char 's' >> fmap Spin parseIntegral
            let pexchange = do
                    _ <- P.char 'x'
                    i <- parseIntegral
                    _<- P.char '/'
                    j <- parseIntegral
                    return $ Exchange i j
            let ppartner = do
                    _ <- P.char 'p'
                    a <- parseProg
                    _<- P.char '/'
                    b <- parseProg
                    return $ Partner a b

            P.choice [pspin, pexchange, ppartner]

main :: IO ()
main = do
    input <- map (either error id . parseMove) . wordsBy (/=',') <$> getLine
    let dance = makeDance 16 input

    putStr "Solution 1: "
    putStrLn $ printDance dance

    putStr "Solution 2: "
    putStrLn $ printDance $ repeatDanceNLarge 1000000000 dance
