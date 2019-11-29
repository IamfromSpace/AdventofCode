{-# LANGUAGE FlexibleInstances #-}

module AdventOfCode.Util
    ( aStar
    , trace
    , traceShow
    , elmTrace
    , labelTrace
    , manDist
    , Manhattan
    , applyNTimes
    , asCounted
    , boundedUntilWithCount
    , boundedUntil
    , listToIndexMap
    ) where

import Control.Monad.Loops (iterateWhile)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.List (partition, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow)

-- I just never rememeber this syntax ><
-- Repeatedly apply a function it's result n times
applyNTimes :: (a -> a) -> a -> Int -> a
applyNTimes fn init n = iterate fn init !! n

-- Take a function, and then have it count its iterations
asCounted :: (a -> a) -> ((Integer, a) -> (Integer, a))
asCounted fn (i, a) = (i + 1, fn a)

-- Run a function repeatedly until a predicate is met, but throw an error if
-- more than a specified nnumber of iterations is run.  Also, the count is returned.
boundedUntilWithCount :: Integer -> (a -> Bool) -> (a -> a) -> a -> (Integer, a)
boundedUntilWithCount max pred fn x =
    if max <= 0
        then error "Iteration bounds was set at or below 0!"
        else let (iterations, last) =
                     until (\(i, y) -> i >= max || pred y) (asCounted fn) (0, x)
             in if iterations == max && not (pred last)
                    then error "Too many iterations!"
                    else (iterations, last)

-- Run a function repeatedly until a predicate is met, but throw an error if
-- more than a specified nnumber of iterations is run.
boundedUntil :: Integer -> (a -> Bool) -> (a -> a) -> a -> a
boundedUntil max pred fn = snd . boundedUntilWithCount max pred fn

elmTrace :: Show a => a -> a
elmTrace x = traceShow x x

labelTrace :: Show a => String -> a -> a
labelTrace label x = traceShow (label, x) x

class Manhattan a where
    manDist :: a -> a -> Integer

instance Manhattan Integer where
    manDist a b = abs (b - a)

instance Manhattan (Integer, Integer) where
    manDist (a0, a1) (b0, b1) = abs (b0 - a0) + abs (b1 - a1)

instance Manhattan (Integer, Integer, Integer) where
    manDist (a0, a1, a2) (b0, b1, b2) =
        abs (b0 - a0) + abs (b1 - a1) + abs (b2 - a2)

instance Manhattan (Integer, Integer, Integer, Integer) where
    manDist (a0, a1, a2, a3) (b0, b1, b2, b3) =
        abs (b0 - a0) + abs (b1 - a1) + abs (b2 - a2) + abs (b3 - a3)

listToIndexMap :: [a] -> Map Int a
listToIndexMap =
    fst . foldl (\(m, i) v -> (Map.insert i v m, i + 1)) (mempty, 0)

aStarStep ::
       (Ord cost, Monoid cost, Ord position)
    => (position -> [(position, (cost, cost))])
    -> State ( Maybe (cost, [position])
             , Map position cost
             , Set ((cost, cost), position, [position])) (Maybe (Maybe ( cost
                                                                       , [position])))
aStarStep getOptions = do
    (shortestKnownPath, seen, queue) <- get
    case Set.minView queue of
        Nothing -> return $ Just shortestKnownPath -- out of explorable points
        Just (((_, prevCost), cheapestGuessPosition, history), queue') -> do
            let nextPossible =
                    filter
                        (\(nextOption, _, (_, pathCost)) ->
                             case Map.lookup nextOption seen of
                                 Nothing -> True
                                 Just lowestCost -> pathCost < lowestCost) $
                    fmap
                        (\(nextOption, (expectedRemaining, stepCost)) ->
                             ( nextOption
                             , cheapestGuessPosition : history
                             , ( expectedRemaining <> stepCost <> prevCost
                               , stepCost <> prevCost))) $
                    getOptions cheapestGuessPosition
            let (done, inProgress) =
                    partition
                        (\(_, _, (totalExpectedCost, pathCost)) ->
                             totalExpectedCost == pathCost)
                        nextPossible
            let shortestKnownPath' =
                    case sortOn
                             (\(_, _, expectedThenCurrentCost) ->
                                  expectedThenCurrentCost)
                             done of
                        ((currentPositionNew, historyNew, (_, pathCostNew)):_) ->
                            case shortestKnownPath of
                                Nothing ->
                                    Just
                                        ( pathCostNew
                                        , currentPositionNew : historyNew)
                                Just (pathCostOld, pathOld) ->
                                    if pathCostNew < pathCostOld
                                        then Just
                                                 ( pathCostNew
                                                 , currentPositionNew :
                                                   historyNew)
                                        else Just (pathCostOld, pathOld)
                        [] -> shortestKnownPath
            let newSearchNodes =
                    filter
                        (\(_, _, (totalExpectedCost, _))
                                 -- This assumes that expected cost is always
                                 -- LESS than the actual cost, must be a BEST case
                          ->
                             case shortestKnownPath' of
                                 Nothing -> True
                                 Just (lowestCost, _) ->
                                     totalExpectedCost <= lowestCost)
                        inProgress
            let seen' =
                    Map.fromList
                        (fmap
                             (\(currentPosition, _, (_, pathCost)) ->
                                  (currentPosition, pathCost))
                             newSearchNodes -- should this be nextPossible?
                         )
                    -- note that maps don't merge values with <>, they merge with const
                    -- we take advantage of this here, since we already filtered out anything
                    -- more costly than what was already there.
                     <>
                    seen
            let queue'' =
                    queue' <>
                    Set.fromList
                        (fmap
                             (\(currentPosition, history, pathCost) ->
                                  (pathCost, currentPosition, history))
                             newSearchNodes)
            put (shortestKnownPath', seen', queue'')
            return Nothing

aStar ::
       (Ord cost, Ord position, Monoid cost)
    => (position -> [(position, (cost, cost))])
    -> position
    -> Maybe (cost, [position])
aStar getOptions pInit =
    fromJust $
    evalState
        (iterateWhile isNothing (aStarStep getOptions))
        (Nothing, mempty, Set.singleton (mempty, pInit, []))
