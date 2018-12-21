module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Data.Bits
import Data.Map
       (Map, findMax, foldrWithKey, fromList, insert, lookup)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

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
    | GreaterThanIR Int
                    Int
                    Int
    | GreaterThanRR Int
                    Int
                    Int
    | EqualToRI Int
                Int
                Int
    | EqualToRR Int
                Int
                Int
    | BinAndI Int
              Int
              Int
    | BinOrI Int
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
        ["gtir", a, b, c] -> GreaterThanIR (read a) (read b) (read c)
        ["gtrr", a, b, c] -> GreaterThanRR (read a) (read b) (read c)
        ["eqri", a, b, c] -> EqualToRI (read a) (read b) (read c)
        ["eqrr", a, b, c] -> EqualToRR (read a) (read b) (read c)
        ["bani", a, b, c] -> BinAndI (read a) (read b) (read c)
        ["bori", a, b, c] -> BinOrI (read a) (read b) (read c)

type Parsed = (Int, [Inst])

parse1 :: String -> Parsed
parse1 s =
    let (h:t) = lines s
        [_, is] = words h
    in (read is, fmap parseLine t)

parse2 :: String -> Parsed
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
        GreaterThanIR a br cr ->
            let b = readReg br rs
                v =
                    if a > b
                        then 1
                        else 0
            in insert cr v rs
        GreaterThanRR ar br cr ->
            let a = readReg ar rs
                b = readReg br rs
                v =
                    if a > b
                        then 1
                        else 0
            in insert cr v rs
        EqualToRI ar b cr ->
            let a = readReg ar rs
                v =
                    if a == b
                        then 1
                        else 0
            in insert cr v rs
        EqualToRR ar br cr ->
            let a = readReg ar rs
                b = readReg br rs
                v =
                    if a == b
                        then 1
                        else 0
            in insert cr v rs
        BinAndI ar b cr ->
            let a = readReg ar rs
            in insert cr (a .&. b) rs
        BinOrI ar b cr ->
            let a = readReg ar rs
            in insert cr (a .|. b) rs

step :: Map Int Inst -> Int -> (Int, Registers) -> ((Int, Registers), Bool)
step insts ipR (ip, regs) =
    if ip == 17
          -- shortcut our otherwise very expensive division!
        then ( (8, insert 2 ((`div` 256) $ fromJust $ lookup 2 regs) regs)
             , False)
        else case lookup ip insts of
                 Nothing -> error "did not expect to halt!"
                 Just inst ->
                     let regs' = insert ipR ip regs
                         regs'' = execute inst regs'
                         ip' = readReg ipR regs'' + 1
                     in ((ip', regs''), ip' == 28)

stepUntil :: Int -> Map Int Inst -> Int -> (Int, Registers) -> (Int, Int)
stepUntil i insts ipR curr =
    let (next@(_, regs'), done) = step insts ipR curr
    in if done
           then (i, readReg 3 regs')
           else stepUntil (i + 1) insts ipR next

answer1 :: Parsed -> String
answer1 (ipR, insts) =
    let instsMap = fromList $ zip [0 .. length insts - 1] insts
    in show $ stepUntil 0 instsMap ipR (0, mempty)

greatestValue :: Map Int Int -> (Int, Int)
greatestValue =
    foldrWithKey
        (\k v (k', v') ->
             if v > v'
                 then (k, v)
                 else (k', v'))
        (0, 0)

stepUntil2 ::
       Int
    -> Map Int Int
    -> Map Int Inst
    -> Int
    -> (Int, Registers)
    -> (Int, Int)
stepUntil2 i seen insts ipR curr =
    let (next@(_, regs'), at28) = step insts ipR curr
    in if at28
           then let v = readReg 3 regs'
                in case lookup v seen of
                       Just _ -> greatestValue seen
                       Nothing ->
                           stepUntil2 (i + 1) (insert v i seen) insts ipR next
           else stepUntil2 (i + 1) seen insts ipR next

answer2 :: Parsed -> String
answer2 (ipR, insts) =
    let instsMap = fromList $ zip [0 .. length insts - 1] insts
    in show $ stepUntil2 0 mempty instsMap ipR (0, mempty)
