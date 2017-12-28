module Main where

import Common.KnotHash
import Control.Monad.State
import Data.Bits (popCount, testBit)
import Data.Word (Word8)
import Data.Foldable (foldlM, forM_)

import qualified Data.Vector.Unboxed as VU


type Memory = VU.Vector Word8

-- Connected Counter State =
-- (matrix of component # for each memory cell, vector of components tree)
type CCState = (VU.Vector Int, VU.Vector Int)

memoryGrid :: String -> Memory
memoryGrid seed = VU.concat $ map genRow [0..127 :: Int]
    where
        genRow i = VU.fromList $ khHashStr $ seed ++ "-" ++ show i

-- For each bit it calls:
-- action i bit bleft btop
--     i = index of the bit
--     bit = value of the bit
--     bleft = value of the left bit (i - 1)
--     btop = value of the top bit (i - 128)
walkMemory :: (Monad m)
           => Memory
           -> (Int -> Bool -> Bool -> Bool -> m ())
           -> m ()
walkMemory mem action = do
    let foldlM' acc ts f = foldlM f acc ts

    forM_ [0,16..2032] $ \row ->
        foldlM' False [0..15] $ \bleft' col -> do
            let byte = mem VU.! (row + col)
            let byteTop = if row > 0 then mem VU.! (row + col - 16) else 0

            foldlM' bleft' [0..7] $ \bleft b -> do
                let bit = testBit byte $ 7 - b
                let btop = testBit byteTop $ 7 - b

                action ((row + col) * 8 + b) bit bleft btop
                return bit

initialCCState :: CCState
initialCCState = (VU.replicate (128*128) 0, VU.empty)

-- Finds connected components by looking at each bit, the bit on its left and
-- its top.
connectedComponents :: Int -> Bool -> Bool -> Bool -> State CCState ()

-- Ignore 0 bits
connectedComponents _ False _ _ = return ()

-- Neighours are 0, make new component
connectedComponents i True False False = do
    (vs, comps) <- get
    let c = VU.length comps
    let comps' = VU.snoc comps c
    let vs' = vs VU.// [(i, c)]
    put (vs', comps')

-- One neighbour is 1, add the bit to that component
connectedComponents i True True False = do
    (vs, comps) <- get
    let c = vs VU.! (i - 1)
    let vs' = vs VU.// [(i, c)]
    put (vs', comps)

-- One neighbour is 1, add the bit to that component
connectedComponents i True False True = do
    (vs, comps) <- get
    let c = vs VU.! (i - 128)
    let vs' = vs VU.// [(i, c)]
    put (vs', comps)

-- Both neighbours are 1, unify the components
connectedComponents i True True True = do
        (vs, comps) <- get
        let c1' = vs VU.! (i - 1)
        let c2' = vs VU.! (i - 128)
        let c1 = lookupComp comps c1'
        let c2 = lookupComp comps c2'
        let cmin = min c1 c2
        put $ if c1' == c2'
            then (vs VU.// [(i, c1')], comps)
            else
                ( vs VU.// [(i, cmin)]
                , comps VU.// [(max c1 c2, cmin), (c1', cmin), (c2', cmin)]
                )
    where
        lookupComp comps a = if a == b then a else lookupComp comps b
            where b = comps VU.! a

main :: IO ()
main = do
    input <- getLine

    let memory = memoryGrid input
    let pc = VU.sum $ VU.map popCount memory
    let (_, comps) =
            execState (walkMemory memory connectedComponents) initialCCState

    putStr "Solution 1: "
    print pc

    -- Print root components
    putStr "Solution 2: "
    print $ VU.length $ VU.ifilter (==) comps
