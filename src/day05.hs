module Main where

import Control.Monad.ST
import Data.Foldable (foldlM)

import qualified Data.Vector.Unboxed.Mutable as VUM


findExit :: (Foldable t, Integral a, VUM.Unbox a) => (a -> a) -> t a -> Int
findExit f t = runST $ do
    let len = length t
    v <- VUM.new len
    _ <- foldlM (\i x -> VUM.write v i x >> return (i + 1)) 0 t

    r <- fixST $ \rec -> return $
        \(i, cnt) -> if i < 0 || i >= len then return cnt else do
            x <- VUM.read v i
            VUM.write v i $ f x
            rec (i + fromIntegral x, cnt + 1)

    r (0, 0)

main :: IO ()
main = do
    input <- ((map read . lines) <$> getContents) :: IO [Int]

    putStr "Solution 1: "
    print $ findExit (+1) input

    putStr "Solution 2: "
    print $ findExit (\x -> if x == 3 then x - 1 else x + 1) input
