module AdventOfCode.Util
    ( aStar
    , trace
    , traceShow
    , elmTrace
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

elmTrace :: Show a => a -> a
elmTrace x = traceShow x x

aStarStep ::
       (Ord cost, Monoid cost, Ord position)
    => (position -> [(position, (cost, cost))])
    -> State ( Maybe (cost, [position])
             , Map position cost
             , Set ((cost, cost), [position])) (Maybe (Maybe (cost, [position])))
aStarStep getOptions = do
    (shortestKnownPath, seen, queue) <- get
    case Set.minView queue of
        Nothing -> return $ Just shortestKnownPath -- out of explorable points
        Just (((_, prevCost), ps), queue') -> do
            let nextPossible =
                    filter
                        (\(p, _, (_, pathCost)) ->
                             case Map.lookup p seen of
                                 Nothing -> True
                                 Just lowestCost -> pathCost < lowestCost) $
                    fmap
                        (\(p, (expectedRemaining, stepCost)) ->
                             ( p
                             , ps
                             , ( expectedRemaining <> stepCost <> prevCost
                               , stepCost <> prevCost))) $
                    -- TODO: We can get rid of this head if we stick with the (h, t) strategy
                    getOptions $ head ps
            let (done, inProgress) =
                    partition
                        (\(_, _, (totalExpectedCost, pathCost)) ->
                             totalExpectedCost == pathCost)
                        nextPossible
            let shortestKnownPath' =
                    case sortOn (\(_, _, c) -> c) done of
                        ((pNew, psNew, (_, cNew)):_) ->
                            case shortestKnownPath of
                                Nothing -> Just (cNew, pNew : psNew)
                                Just (cOld, psOld) ->
                                    if cNew < cOld
                                        then Just (cNew, pNew : psNew)
                                        else Just (cOld, psOld)
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
                        (fmap (\(h, _, (_, c)) -> (h, c)) newSearchNodes) -- should this be nextPossible?
                    -- note that maps don't merge values with <>, they merge with const
                    -- we take advantage of this here, since we already filtered out anything
                    -- more costly than what was already there.
                     <>
                    seen
            let queue'' =
                    queue' <>
                    Set.fromList
                        (fmap (\(p, ps, c) -> (c, p : ps)) newSearchNodes)
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
        (Nothing, mempty, Set.singleton (mempty, [pInit]))
