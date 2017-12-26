{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Maybe (fromJust)
import Common.Parsers
import Control.Applicative ((<|>))
import Control.Monad.State

import qualified Data.Map as M
import qualified Text.Parsec as P


type CPUWord = Int

data Operand
    = Reg String
    | Const CPUWord
    deriving (Show)

data Comparison
    = CGT
    | CGE
    | CEQ
    | CNE
    | CLE
    | CLT
    deriving (Show)

data Condition = Condition
    { condComp :: Comparison
    , condOp1 :: Operand
    , condOp2 :: Operand
    }
    deriving (Show)

data Instruction
    = Add String CPUWord Condition
    deriving (Show)

data CPU = CPU
    { cpuRegs :: M.Map String CPUWord
    , cpuMaxReg :: Maybe Word
    }
    deriving (Show)

parseOperand :: P.Stream s m Char => P.ParsecT s u m Operand
parseOperand = (Reg <$> P.many1 P.letter) <|> (Const <$> parseIntegral)

parseInstr :: String -> Either String Instruction
parseInstr = perrToStr . P.parse p "parseInstr"
    where
        perrToStr (Left err) = Left $ show err
        perrToStr (Right x) = Right x
        comps = zip [">=", ">", "==", "!=", "<=", "<"]
                    [CGE,  CGT, CEQ,  CNE,  CLE,  CLT]
        instrs = zip ["inc", "dec"] [1, -1]
        p = do
            reg <- P.many1 P.letter
            P.spaces

            op <- P.choice $ map (P.try . P.string . fst) instrs
            P.spaces
            ammount <- parseIntegral
            P.spaces

            _ <- P.string "if"
            P.spaces
            comp_a <- parseOperand
            P.spaces
            comp <- P.choice $ map (P.try . P.string . fst) comps
            P.spaces
            comp_b <- parseOperand

            let Just cond = (\f -> Condition f comp_a comp_b) <$> lookup comp comps
            let Just instr = (\f -> Add reg (f * ammount) cond) <$> lookup op instrs

            return instr

runComparison :: Ord a => Comparison -> a -> a -> Bool
runComparison CGT = (>)
runComparison CGE = (>=)
runComparison CEQ = (==)
runComparison CNE = (/=)
runComparison CLE = (<=)
runComparison CLT = (<)

getRegister :: String -> State CPU CPUWord
getRegister reg = gets (M.findWithDefault 0 reg . cpuRegs)

getValue :: Operand -> State CPU CPUWord
getValue (Reg reg) = getRegister reg
getValue (Const val) = return val

modifyRegister :: String -> Int -> State CPU ()
modifyRegister reg delta = modify (\cpu -> let
            newVal = delta + M.findWithDefault 0 reg (cpuRegs cpu)
            newMax = maybe newVal (max newVal . fromIntegral) $ cpuMaxReg cpu
            newRegs = M.insert reg newVal $ cpuRegs cpu
        in
            cpu{cpuRegs = newRegs, cpuMaxReg = Just $ fromIntegral newMax}
    )

checkCondition :: Condition -> State CPU Bool
checkCondition (Condition comp op1 op2) =
    liftM2 (runComparison comp) (getValue op1) (getValue op2)

runInstr :: Instruction ->  State CPU ()
runInstr (Add reg delta cond) = do
    b <- checkCondition cond
    when b $ modifyRegister reg delta

runProgram :: [Instruction] -> CPU
runProgram instrs = execState (forM instrs runInstr) (CPU M.empty Nothing)

main :: IO ()
main = do
    input_raw <- lines <$> getContents
    let input = map (either error id . parseInstr) input_raw

    let finalCPU = runProgram input

    putStr "Solution 1: "
    print $ maximum $ cpuRegs finalCPU

    putStr "Solution 2: "
    print $ fromJust $ cpuMaxReg finalCPU
