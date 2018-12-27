module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , listToIndexMap
    ) where

import Data.Bits
import qualified Data.List
import Data.List.Split (splitOn, splitOneOf)
import Data.Map (Map, filter, insert, keys)
import Debug.Trace (traceShow)
import GHC.Exts (groupWith)
import Prelude hiding (filter)

type Registers = (Int, Int, Int, Int)

type Instruction = (Int, (Int, Int, Int))

type Op = (Int -> Int -> Int, Bool, Bool)

type Instruction' = (Op, (Int, Int, Int))

type CpuTest = (Registers, Instruction, Registers)

listTo4Tuple :: [a] -> (a, a, a, a)
listTo4Tuple [a, b, c, d] = (a, b, c, d)

parseRegisters :: String -> Registers
parseRegisters = listTo4Tuple . fmap read . take 4 . drop 1 . splitOneOf "[],"

parseInstruction :: String -> Instruction
parseInstruction s =
    let [a, b, c, d] = read <$> words s
    in (a, (b, c, d))

parseCpuTest :: String -> CpuTest
parseCpuTest s =
    let [left, mid, right] = lines s
    in (parseRegisters left, parseInstruction mid, parseRegisters right)

parse1 :: String -> [CpuTest]
parse1 s =
    let [left, _] = splitOn "\n\n\n" s
        cpuTestStrs = splitOn "\n\n" left
    in fmap parseCpuTest cpuTestStrs

type Execution = Instruction -> Registers -> Registers

bToI :: Bool -> Int
bToI b =
    if b
        then 1
        else 0

readReg :: Int -> Registers -> Int
readReg 0 (x, _, _, _) = x
readReg 1 (_, x, _, _) = x
readReg 2 (_, _, x, _) = x
readReg 3 (_, _, _, x) = x

writeReg :: Int -> Int -> Registers -> Registers
writeReg 0 x (_, b, c, d) = (x, b, c, d)
writeReg 1 x (a, _, c, d) = (a, x, c, d)
writeReg 2 x (a, b, _, d) = (a, b, x, d)
writeReg 3 x (a, b, c, _) = (a, b, c, x)

execute :: Instruction' -> Registers -> Registers
execute ((op, aIsImmediate, bIsImmediate), (aq, bq, cr)) rs =
    let a =
            if aIsImmediate
                then aq
                else readReg aq rs
        b =
            if bIsImmediate
                then bq
                else readReg bq rs
    in writeReg cr (a `op` b) rs

addi :: Op
addi = ((+), False, True)

addr :: Op
addr = ((+), False, False)

muli :: Op
muli = ((*), False, True)

mulr :: Op
mulr = ((*), False, False)

bani :: Op
bani = ((.&.), False, True)

banr :: Op
banr = ((.&.), False, False)

bori :: Op
bori = ((.|.), False, True)

borr :: Op
borr = ((.|.), False, False)

seti :: Op
seti = (const, True, True)

setr :: Op
setr = (const, False, False)

gtir :: Op
gtir = (\a b -> bToI (a > b), True, False)

gtri :: Op
gtri = (\a b -> bToI (a > b), False, True)

gtrr :: Op
gtrr = (\a b -> bToI (a > b), False, False)

eqir :: Op
eqir = (\a b -> bToI (a == b), True, False)

eqri :: Op
eqri = (\a b -> bToI (a == b), False, True)

eqrr :: Op
eqrr = (\a b -> bToI (a == b), False, False)

ops :: [Op]
ops =
    [ addi
    , addr
    , muli
    , mulr
    , bani
    , banr
    , bori
    , borr
    , seti
    , setr
    , gtir
    , gtri
    , gtrr
    , eqir
    , eqri
    , eqrr
    ]

answer1 :: [CpuTest] -> Int
answer1 cpuTests =
    let test (before, (_, info), after) =
            sum (fmap (\op -> bToI (execute (op, info) before == after)) ops) >=
            3
    in sum $ fmap (bToI . test) cpuTests

type Parsed2 = ([CpuTest], [Instruction])

parse2 :: String -> Parsed2
parse2 s =
    let [left, right] = splitOn "\n\n\n\n" s
        cpuTestStrs = splitOn "\n\n" left
        cpuTests = fmap parseCpuTest cpuTestStrs
        insts = parseInstruction <$> lines right
    in (cpuTests, insts)

execute' :: [Op] -> Instruction -> Registers -> Registers
execute' ops (i, info) = execute (ops !! i, info)

runCpuTest :: [Op] -> CpuTest -> Bool
runCpuTest ops (before, inst, after) = after == execute' ops inst before

runCpuTests :: [Op] -> [CpuTest] -> Bool
runCpuTests ops = all (runCpuTest ops)

runCpuTest' :: Op -> CpuTest -> Bool
runCpuTest' op (before, (_, info), after) = after == execute (op, info) before

runCpuTests' :: Op -> [CpuTest] -> Bool
runCpuTests' op = all (runCpuTest' op)

listToIndexMap :: [a] -> Map Int a
listToIndexMap l =
    let len = length l
    in snd $ foldr (\a (i, m) -> (i - 1, insert i a m)) (len - 1, mempty) l

getPossibleOps :: [CpuTest] -> [[Int]]
getPossibleOps cpuTests =
    (\cpuTestsSameOpCode ->
         keys $ filter (`runCpuTests'` cpuTestsSameOpCode) $ listToIndexMap ops) <$>
    groupWith (\(_, (x, _), _) -> x) cpuTests

correctPermutation = [13, 7, 1, 11, 2, 10, 3, 5, 6, 14, 15, 4, 9, 12, 0, 8]

-- Lazy and did this by hand from getPossibleOps
correctOps :: [Op]
correctOps =
    [ eqir
    , borr
    , addr
    , gtri
    , muli
    , gtir
    , mulr
    , banr
    , bori
    , eqri
    , eqrr
    , bani
    , setr
    , gtrr
    , addi
    , seti
    ]

answer2 :: Parsed2 -> Int
answer2 (cpuTests, instructions) =
    (\(x, _, _, _) -> x) $
    foldl (flip (execute' correctOps)) (0, 0, 0, 0) instructions
