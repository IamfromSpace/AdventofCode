module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , findReady
    , allSteps
    , orderDeps
    , toMapOfSets
    ) where

import Data.Char (ord)
import Data.List (foldr)
import Data.Map (Map, insertWith, lookup)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set
       (Set, delete, difference, findMin, foldr, fromList, insert,
        minView, notMember, null, singleton, union)
import Debug.Trace

data Letter
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z

parse1 :: String -> [(Char, Char)]
parse1 =
    fmap
        (\s ->
             let [_, dep, _, _, _, _, _, k, _, _] = words s
             in (head k, head dep)) .
    lines

parse2 :: String -> [(Char, Char)]
parse2 = parse1

toMapOfSets :: [(Char, Char)] -> Map Char (Set Char)
toMapOfSets =
    Data.List.foldr (\(k, dep) m -> insertWith union k (singleton dep) m) mempty

allSteps :: [(Char, Char)] -> Set Char
allSteps = Data.List.foldr (\(k, dep) m -> insert dep $ insert k m) mempty

findReady :: Set Char -> Set Char -> Map Char (Set Char) -> Set Char
findReady readyNow candidates dependencies =
    Data.Set.foldr
        (\candidate s ->
             let depSet =
                     fromMaybe mempty $ Data.Map.lookup candidate dependencies
             in if notMember candidate readyNow &&
                   Data.Set.null (difference depSet readyNow)
                    then insert candidate s
                    else s)
        mempty
        candidates

orderDeps' :: String -> Set Char -> Set Char -> Map Char (Set Char) -> String
orderDeps' built readyNow allChars dependencies =
    let remaining = difference allChars readyNow
    in if Data.Set.null remaining
           then built
           else let next = findMin $ findReady readyNow remaining dependencies
                in orderDeps'
                       (built ++ [next])
                       (insert next readyNow)
                       allChars
                       dependencies

orderDeps :: Set Char -> Map Char (Set Char) -> String
orderDeps = orderDeps' "" mempty

answer1 :: [(Char, Char)] -> String
answer1 x = orderDeps (allSteps x) (toMapOfSets x)

type Elf = (Int, Maybe Char)

completeWork :: Elf -> (Elf, Maybe Char)
completeWork (tMinus, mChar) =
    let tMinus' = tMinus - 1
    in if tMinus' == 0
           then ((1, Nothing), mChar)
           else ((tMinus', mChar), Nothing)

takeWork :: Set Char -> Elf -> (Elf, Set Char)
takeWork available elf@(tMinus, mInProgChar) =
    case mInProgChar of
        Just _ -> (elf, available)
        Nothing ->
            let mNext = minView available
            in case mNext of
                   Just (char, available')
                       -- 64 for example
                    -> ((ord char - 4, Just char), available')
                   Nothing -> (elf, available)

workAll' ::
       Int
    -> (Elf, Elf, Elf, Elf, Elf)
    -> Set Char
    -> Set Char
    -> Map Char (Set Char)
    -> Int
workAll' t (worker1, worker2, worker3, worker4, worker5) readyNow allChars dependencies =
    if Data.Set.null (difference allChars readyNow) || t > 10000
        then t - 1
        else let (worker1', done1) = completeWork worker1
                 (worker2', done2) = completeWork worker2
                 (worker3', done3) = completeWork worker3
                 (worker4', done4) = completeWork worker4
                 (worker5', done5) = completeWork worker5
                 readyNow' =
                     readyNow `union`
                     fromList (catMaybes [done1, done2, done3, done4, done5])
                 inProgress =
                     fromList $
                     catMaybes
                         [ snd worker1'
                         , snd worker2'
                         , snd worker3'
                         , snd worker4'
                         , snd worker5'
                         ]
                 remaining = difference allChars (readyNow' `union` inProgress)
                 available = findReady readyNow' remaining dependencies
                 (worker1'', available') = takeWork available worker1'
                 (worker2'', available'') = takeWork available' worker2'
                 (worker3'', available''') = takeWork available'' worker3'
                 (worker4'', available'''') = takeWork available''' worker4'
                 (worker5'', available''''') = takeWork available'''' worker5'
             in workAll'
                    (t + 1)
                    (worker1'', worker2'', worker3'', worker4'', worker5'')
                    readyNow'
                    allChars
                    dependencies

workAll :: Set Char -> Map Char (Set Char) -> Int
workAll =
    workAll'
        0
        ((1, Nothing), (1, Nothing), (1, Nothing), (1, Nothing), (1, Nothing))
        mempty

answer2 :: [(Char, Char)] -> Int
answer2 x = workAll (allSteps x) (toMapOfSets x)
