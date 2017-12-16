{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.State

import qualified Data.Map as M
import qualified Text.Parsec as P


data Operand
    = Reg String
    | Const Int
    deriving (Show)

data Condition
    = CGT Operand Operand
    | CGE Operand Operand
    | CEQ Operand Operand
    | CNE Operand Operand
    | CLE Operand Operand
    | CLT Operand Operand
    deriving (Show)

data Instruction
    = Inc String Int Condition
    | Dec String Int Condition
    deriving (Show)

newtype CPU = CPU
    { cpuRegs :: M.Map String Int
    }

parseInt :: (P.Stream s m Char, Read a, Integral a) => P.ParsecT s u m a
parseInt = do
    signs <- P.many $ P.oneOf "+-"
    magnitude <- P.many1 P.digit

    let sign = foldl (\a b -> if a == b then '+' else '-') '+' signs

    return $ case sign of
        '+' -> read magnitude
        '-' -> - read magnitude
        _ -> undefined

parseOperand :: P.Stream s m Char => P.ParsecT s u m Operand
parseOperand = (Reg <$> P.many1 P.letter) <|> (Const <$> parseInt)

parseInstr :: String -> Either String Instruction
parseInstr = perrToStr . P.parse p "parseInstr"
    where
        perrToStr (Left err) = Left $ show err
        perrToStr (Right x) = Right x
        comps = zip [">=", ">", "==", "!=", "<=", "<"]
                    [CGE, CGT,  CEQ,  CNE,  CLE,  CLT]
        instrs = zip ["inc", "dec"] [Inc, Dec]
        p = do
            reg <- P.many1 P.letter
            P.spaces

            op <- P.choice $ map (P.try . P.string . fst) instrs
            P.spaces
            ammount <- parseInt
            P.spaces

            _ <- P.string "if"
            P.spaces
            comp_a <- parseOperand
            P.spaces
            comp <- P.choice $ map (P.try . P.string . fst) comps
            P.spaces
            comp_b <- parseOperand

            let Just cond = (\f -> f comp_a comp_b) <$> lookup comp comps
            let Just instr = (\f -> f reg ammount cond) <$> lookup op instrs

            return instr

getRegister :: String -> State CPU Int
getRegister reg = gets (M.findWithDefault 0 reg . cpuRegs)

modifyRegister :: String -> Int -> State CPU ()
modifyRegister reg delta =
    modify (\cpu -> cpu{cpuRegs = M.insertWith (+) reg delta $ cpuRegs cpu})

runInstr :: Instruction ->  State CPU ()
runInstr = undefined

main :: IO ()
main = do
    input_raw <- lines <$> getContents
    let input = map (either error id . parseInstr) input_raw

    print input
