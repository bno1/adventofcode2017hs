module Common
    ( wordsBy
    ) where

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy f = words . map (\c -> if f c then c else ' ')
