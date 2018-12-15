{-# LANGUAGE BangPatterns #-}

module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , generation
    , generations
    ) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Set
       (Set, findMax, findMin, fold, fromList, insert, mapMonotonic,
        member, singleton, toList)
import Debug.Trace (traceShow)

type Config = (Bool, Bool, Bool, Bool, Bool)

type Parsed1 = (Set Int, Set Config)

trueIndexes :: Int -> [Bool] -> [Int]
trueIndexes i (h:t) =
    if h
        then i : trueIndexes (i + 1) t
        else trueIndexes (i + 1) t
trueIndexes _ [] = []

parseRule :: String -> Maybe Config
parseRule s =
    let [rule, _, planted] = words s
        (a:b:c:d:e:_) = fmap (== '#') rule
    in if planted == "#"
           then Just (a, b, c, d, e)
           else Nothing

parse1 :: String -> Parsed1
parse1 s =
    let (h:_:t) = lines s
        [_, _, stateStr] = words h
        plantState = fmap (== '#') stateStr
    in ( fromList $ trueIndexes 0 plantState
       , fromList $ catMaybes $ fmap parseRule t)

getSetBounds :: Set a -> (a, a)
getSetBounds m = (findMin m, findMax m)

willGrow :: Set Config -> Set Int -> Int -> Bool
willGrow rules state index =
    member
        ( member (index - 2) state
        , member (index - 1) state
        , member index state
        , member (index + 1) state
        , member (index + 2) state)
        rules

generation :: Set Config -> Set Int -> Set Int
generation rules state =
    let (!min, !max) = getSetBounds state
    in foldr
           (\i !nextState ->
                if willGrow rules state i
                    then insert i nextState
                    else nextState)
           mempty
           [min - 2 .. max + 2]

generations :: Int -> Set Config -> Set Int -> Set Int
generations 0 rules state = state
generations !i !rules !state =
    if mod i 10000 == 0
        then traceShow i $ generations (i - 1) rules (generation rules state)
        else generations (i - 1) rules (generation rules state)

scoot :: Set Int -> Set Int
scoot s =
    let min = findMin s
    in mapMonotonic (\x -> x - min) s

-- Seems to be no less doomed
findCycle :: Int -> M.Map (Set Int) Int -> Set Config -> Set Int -> (Int, Int)
findCycle 0 seen rules state =
    findCycle 1 (M.singleton (scoot state) 0) rules state
findCycle !i !seen !rules !state =
    let next = generation rules state
        scooted = scoot next
    in case M.lookup scooted seen of
           Just x -> (x, i)
           Nothing -> findCycle (i + 1) (M.insert scooted i seen) rules next

parse2 :: String -> Parsed1
parse2 = parse1

answer1 :: Parsed1 -> String
answer1 = show . sum . toList . uncurry (flip (generations 20))

-- Remainder done by hand after finding the cycle
answer2 :: Parsed1 -> String
answer2 = show . sum . toList . uncurry (flip (generations 125))
 -- = show . (\(a, b) -> b - a) . getSetBounds . uncurry (flip (generations 39))
 -- = show . uncurry (flip (findCycle 0 mempty))
