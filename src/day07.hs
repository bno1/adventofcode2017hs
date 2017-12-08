module Main where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.List
import Data.Maybe (fromJust, fromMaybe)

import qualified Text.Parsec as P
import qualified Data.Map as M


data Tree = Empty
          | Node String Int [Tree]
          deriving (Show, Eq)

data ProgDescr = ProgDescr
    { progParent :: String
    , progWeight :: Int
    , progChildren :: [String]
    } deriving (Show)

progDescr :: ProgDescr
progDescr = ProgDescr {progParent = "", progWeight = 0, progChildren = []}

alterProg :: Int -> [String] -> Maybe ProgDescr -> ProgDescr
alterProg w chld =
    (\e -> e{progWeight = w, progChildren = chld}) . fromMaybe progDescr

alterProgParent :: String -> Maybe ProgDescr -> ProgDescr
alterProgParent parent = (\e -> e{progParent = parent}) . fromMaybe progDescr

findMap :: (a -> Bool) -> M.Map k a -> Maybe (k, a)
findMap f = M.foldrWithKey (\k a r -> if f a then Just (k, a) else r) Nothing

parseLine :: String -> Either String (String, Int, [String])
parseLine = perrToStr . P.parse p "parseLine"
    where
        perrToStr (Left err) = Left $ show err
        perrToStr (Right x) = Right x
        p = do
            prog <- P.many1 P.alphaNum
            P.spaces
            _ <- P.char '('
            weight <- P.many1 P.digit
            _ <- P.char ')'
            children <- P.option [] $ do
                P.spaces
                _ <- P.string "->"
                P.spaces
                P.sepBy1 (P.many1 P.alphaNum) (P.char ',' >> P.spaces)
            P.eof
            return (prog, read weight, children)

buildTree :: [(String, Int, [String])] -> Tree
buildTree lst = tree root
    where
        helper :: (String, Int, [String]) -> State (M.Map String ProgDescr) ()
        helper (prog, weight, chld) = do
            modify $ M.alter (Just . alterProg weight chld) prog
            forM_ chld $ modify . M.alter (Just . alterProgParent prog)

        progMap = execState (forM_ lst helper) M.empty

        root = fst . fromJust $ findMap (null . progParent) progMap

        tree prog = case M.lookup prog progMap of
            Nothing -> Empty
            Just (ProgDescr _ weight chld) -> Node prog weight (map tree chld)

findUnbalanced :: Tree -> Maybe (Int, Int, Tree)
findUnbalanced = snd . helper
    where
        helper Empty = (0, Nothing)
        helper (Node _ w chld) = (weight, xs')
            where
                rec = map helper chld
                weights = map fst rec
                weight_chld = zip weights chld
                weight = w + sum weights
                xs = msum $ map snd rec
                xs' = xs <|> case group . sort $ weights of
                    [] -> Nothing
                    [_] -> Nothing
                    [[act], ref:_] -> (,,) ref act <$> lookup act weight_chld
                    [ref:_, [act]] -> (,,) ref act <$> lookup act weight_chld
                    _ -> error $ "Unexpected weights " ++ show weights

main :: IO ()
main = do
    input_raw <- lines <$> getContents
    let input = map (either error id . parseLine) input_raw

    let tree = buildTree input
    let root@(Node rootProg _ _) = tree
    let Just (expected, actual, Node _ w _) = findUnbalanced root

    putStr "Solution 1: "
    putStrLn rootProg

    putStr "Solution 2: "
    print $ w + (expected - actual)
