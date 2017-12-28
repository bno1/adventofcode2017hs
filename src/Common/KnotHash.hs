{-# LANGUAGE RankNTypes #-}

module Common.KnotHash
    ( khRoundW8
    , khHashW8
    , khHashStr
    ) where

import Data.Bits (xor, zeroBits, Bits)
import Data.Char (ord)
import Data.Word (Word8)
import Control.Monad (unless, forM_)
import Control.Monad.State
import Control.Monad.ST

import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU


-- state vector, start and skip
type KnotHashState s i = (VUM.STVector s i, Int, Int)

type KnotHash i a = forall s. StateT (KnotHashState s i) (ST s) a

makeKnotHashState :: (VU.Unbox i) => [i] -> ST s (KnotHashState s i)
makeKnotHashState items = do
    let len = length items
    vec <- VGM.new len
    forM_ (zip [0..] items) $ uncurry (VGM.write vec)

    return (vec, 0, 0)

evalKnotHash :: (VU.Unbox i) => [i] -> KnotHash i a -> a
evalKnotHash items kh = runST $ makeKnotHashState items >>= evalStateT kh

execKnotHash :: (VU.Unbox i) => [i] -> KnotHash i a -> (VU.Vector i, Int, Int)
execKnotHash items kh = runST $ do
    (vec, start, skip) <- makeKnotHashState items >>= execStateT kh
    fvec <- VU.freeze vec
    return (fvec, start, skip)

khRound :: (VU.Unbox i) => [Int] -> KnotHash i ()
khRound lengths = do
    (vec, start, skip) <- get

    let len = VGM.length vec

    let swap i j c = case () of
            _ | c == 0 -> return ()
              | i == len -> swap 0 j c
              | j == -1 -> swap i (len - 1) c
              | otherwise -> VUM.swap vec i j >> swap (i + 1) (j - 1) (c - 1)

    let step st n = unless (n > len) $
            swap st ((st + n - 1) `rem` len) (n `div` 2)

    (start', skip') <- foldM
        (\(st, sk) n -> do
            step st n
            return ((st + n + sk) `rem` len, sk + 1)
        )
        (start, skip)
        lengths

    put (vec, start', skip')

khCompress :: (VU.Unbox i, Bits i) => KnotHash i [i]
khCompress = do
    (vec, _, _) <- get
    fvec <- VU.freeze vec

    let len = VU.length fvec
    let (q, r) = quotRem len 16

    -- compress all the 16 bytes blocks
    let a = map (\i -> VU.foldl1 xor $ VU.slice (i * 16) 16 fvec) [0..q - 1]

    -- compress the last incomplete block if r > 0
    let b = [VU.foldl xor zeroBits $ VU.slice (q * 16) r fvec | r > 0]

    return $ a ++ b

khHash :: (VU.Unbox i, Bits i) => [Int] -> KnotHash i [i]
khHash items = replicateM_ 64 (khRound items) >> khCompress

khRoundW8 :: [Int] -> [Word8]
khRoundW8 lengths = VU.toList v
    where (v, _, _) = execKnotHash [0..255] $ khRound lengths

khHashW8 :: [Int] -> [Word8]
khHashW8 lengths = evalKnotHash [0..255] $ khHash lengths

khHashStr :: String -> [Word8]
khHashStr str = khHashW8 $ map ord str ++ [17, 31, 73, 47, 23]
