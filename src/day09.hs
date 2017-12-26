module Main where


score :: String -> (Int, Int)
score = group 0 1 0
    where
        group sc _ cnt [] = (sc, cnt)
        group sc lvl cnt ('{':xs) = group (sc + lvl) (lvl + 1) cnt xs
        group sc lvl cnt ('}':xs) = group sc (lvl - 1) cnt xs
        group sc lvl cnt ('<':xs) = trash sc lvl cnt xs
        group sc lvl cnt (_:xs) = group sc lvl cnt xs

        trash sc _ cnt [] = (sc, cnt)
        trash sc lvl cnt ('!':_:xs) = trash sc lvl cnt xs
        trash sc lvl cnt ('>':xs) = group sc lvl cnt xs
        trash sc lvl cnt (_:xs) = trash sc lvl (cnt + 1) xs

main :: IO ()
main = do
    input <- getLine
    let (sc, cnt) = score input

    putStr "Solution 1: "
    print sc

    putStr "Solution 2: "
    print cnt
