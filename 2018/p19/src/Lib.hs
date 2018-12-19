module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Data.Map (Map, fromList, insert, lookup)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

type Parsed1 = (Int, [Inst])

data Inst
    = AddI Int
           Int
           Int
    | AddR Int
           Int
           Int
    | SetI Int
           Int
    | SetR Int
           Int
    | MultI Int
            Int
            Int
    | MultR Int
            Int
            Int
    | GreaterThanR Int
                   Int
                   Int
    | EqualToR Int
               Int
               Int
    deriving (Show)

type Registers = Map Int Int

parseLine :: String -> Inst
parseLine s =
    case words s of
        ["addi", a, b, c] -> AddI (read a) (read b) (read c)
        ["addr", a, b, c] -> AddR (read a) (read b) (read c)
        ["seti", a, b, c] -> SetI (read a) (read c)
        ["setr", a, b, c] -> SetR (read a) (read c)
        ["muli", a, b, c] -> MultI (read a) (read b) (read c)
        ["mulr", a, b, c] -> MultR (read a) (read b) (read c)
        ["gtrr", a, b, c] -> GreaterThanR (read a) (read b) (read c)
        ["eqrr", a, b, c] -> EqualToR (read a) (read b) (read c)

parse1 :: String -> Parsed1
parse1 s =
    let (h:t) = lines s
        [_, is] = words h
    in (read is, fmap parseLine t)

parse2 :: String -> Parsed1
parse2 = parse1

readReg :: Int -> Registers -> Int
readReg i = fromMaybe 0 . lookup i

execute :: Inst -> Registers -> Registers
execute i rs =
    case i of
        AddI ar b cr ->
            let a = readReg ar rs
            in insert cr (a + b) rs
        AddR ar br cr ->
            let a = readReg ar rs
                b = readReg br rs
            in insert cr (a + b) rs
        SetI a cr -> insert cr a rs
        SetR ar cr ->
            let a = readReg ar rs
            in insert cr a rs
        MultI ar b cr ->
            let a = readReg ar rs
            in insert cr (a * b) rs
        MultR ar br cr ->
            let a = readReg ar rs
                b = readReg br rs
            in insert cr (a * b) rs
        GreaterThanR ar br cr ->
            let a = readReg ar rs
                b = readReg br rs
                v =
                    if a > b
                        then 1
                        else 0
            in insert cr v rs
        EqualToR ar br cr ->
            let a = readReg ar rs
                b = readReg br rs
                v =
                    if a == b
                        then 1
                        else 0
            in insert cr v rs

step :: Map Int Inst -> Int -> (Int, Registers) -> ((Int, Registers), Bool)
step insts ipR (ip, regs) =
    case lookup ip insts of
        Nothing -> ((ip, regs), True)
        Just inst ->
            let regs' = insert ipR ip regs
                regs'' = execute inst regs'
                ip' = readReg ipR regs'' + 1
            in ((ip', regs''), False)

stepUntil :: Int -> Map Int Inst -> Int -> (Int, Registers) -> Int
stepUntil i insts ipR curr =
    let (next@(_, regs'), done) = step insts ipR curr
    in if done
           then readReg 0 regs'
           else stepUntil (i + 1) insts ipR next

answer1 :: Parsed1 -> String
answer1 (ipR, insts) =
    let instsMap = fromList $ zip [0 .. length insts - 1] insts
    in show $ stepUntil 0 instsMap ipR (0, mempty)

stepUntil2 :: Int -> Map Int Inst -> Int -> (Int, Registers) -> Int
stepUntil2 i insts ipR curr =
    let (next@(_, regs'), done) = step insts ipR curr
    in if i > 20
           then readReg 4 regs'
           else stepUntil2 (i + 1) insts ipR next

answer2 :: Parsed1 -> String
answer2 (ipR, insts) =
    let instsMap = fromList $ zip [0 .. length insts - 1] insts
        x = stepUntil2 0 instsMap ipR (0, fromList [(0, 1)])
    in show $ sum $ filter (\i -> x `mod` i == 0) [1 .. x]
