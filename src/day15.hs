module Main where

import Data.Bits
import Data.Word (Word64)


parseLine :: String -> String -> Maybe Word64
parseLine name line = case words line of
    ["Generator", name', "starts", "with", n] ->
        if name == name' then Just (read n) else Nothing
    _ -> Nothing

-- Operations are done in Word64 to avoid overflow before applying modulo
generate :: Word64 -> Word64 -> [Word64]
generate f s = map fromIntegral $ iterate ((`rem` 2147483647) . (*f)) s

match :: Word64 -> Word64 -> Bool
match a b = (a .&. 0xffff) == (b .&. 0xffff)

judge :: Int -> [Word64] -> [Word64] -> Int
judge cnt ga gb = length . filter (uncurry match) . take cnt $ zip ga gb

main :: IO ()
main = do
    let fa = 16807
    let fb = 48271

    Just sa <- parseLine "A" <$> getLine
    Just sb <- parseLine "B" <$> getLine

    let gena1 = generate fa sa
    let genb1 = generate fb sb
    let matches1 = judge 40000000 gena1 genb1

    -- Don't use gena1 and genb1 to avoid useless memoization
    -- Otherwise the program takes 3 times longer and consumes insane amounts
    -- of memory trying to memorize and reuse the gena1 and genb2 lists
    let gena2 = filter (\v -> v .&. 0x3 == 0) $ generate fa sa
    let genb2 = filter (\v -> v .&. 0x7 == 0) $ generate fa sb
    let matches2 = judge 5000000 gena2 genb2

    putStr "Solution 1: "
    print matches1

    putStr "Solution 2: "
    print matches2
