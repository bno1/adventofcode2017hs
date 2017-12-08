module Main where

import Data.Maybe (fromJust, fromMaybe)
import Text.Parsec hiding (State, Empty)
import Control.Monad.State
import qualified Data.Map as M

data Tree = Empty
          | Node String Int [Tree]
          deriving (Show, Eq)

data MapEntry = MapEntry String Int [String] deriving (Show)

entry :: MapEntry
entry = MapEntry "" 0 []

initEntry :: Int -> [String] -> MapEntry -> MapEntry
initEntry w chld (MapEntry parent _ _) = MapEntry parent w chld

setParent :: String -> MapEntry -> MapEntry
setParent parent (MapEntry _ w chld) = MapEntry parent w chld

lineParse :: String -> Either String (String, Int, [String])
lineParse = parseErr . parse p "lineParse"
    where
        parseErr (Left err) = Left $ show err
        parseErr (Right x) = Right x
        p = do
            prog <- many1 alphaNum
            spaces
            _ <- char '('
            weight <- many1 digit
            _ <- char ')'
            children <- option [] $ do
                spaces
                _ <- string "->"
                spaces
                sepBy1 (many1 alphaNum) (char ',' >> spaces)
            eof
            return (prog, read weight, children)

buildTree :: [(String, Int, [String])] -> Tree
buildTree lst = tree root
    where
        helper :: (String, Int, [String]) -> State (M.Map String MapEntry) ()
        helper (prog, weight, chld) = do
            modify $ M.alter (Just . initEntry weight chld . fromMaybe entry) prog
            forM_ chld $ modify . M.alter (Just . setParent prog. fromMaybe entry)
        emap = execState (forM_ lst helper) M.empty
        root = fromJust $ M.foldrWithKey (\k (MapEntry p _ _) acc -> if null p then Just k else acc) Nothing emap
        tree prog = case M.lookup prog emap of
            Nothing -> Empty
            Just (MapEntry _ weight chld) -> Node prog weight (map tree chld)

checkBalance :: Tree -> Int
checkBalance = undefined
--checkBalance (Node _ w chld) = map checkBalance chld

main :: IO ()
main = do
    input_raw <- lines <$> getContents
    let input = map (either error id . lineParse) input_raw

    let tree = buildTree input
    let (Node root _ _) = tree

    print root
    return ()
