module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Control.Applicative (liftA2)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.List (partition, sortOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Monoid (Sum(Sum))
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace (traceShow)

elmTrace :: Show a => a -> a
elmTrace x = traceShow x x

type Parsed1 = (Int, (Int, Int))

parse1 :: String -> Parsed1
parse1 _ = (11817, (9, 751))

magicModulo :: Int
magicModulo = 20183

magicX :: Int
magicX = 48271

magicY :: Int
magicY = 16807

insertErosionLevel ::
       (Int, (Int, Int))
    -> (Int, Int)
    -> Map (Int, Int) Int
    -> Map (Int, Int) Int
insertErosionLevel (depth, target) p erosionLevels =
    let geoIndex =
            if p == target
                then 0
                else case p of
                         (0, 0) -> 0
                         (x, 0) -> x * magicY
                         (0, y) -> y * magicX
                         (x, y) ->
                             let mGeoIndex =
                                     liftA2
                                         (*)
                                         (Map.lookup (x - 1, y) erosionLevels)
                                         (Map.lookup (x, y - 1) erosionLevels)
                             in fromMaybe
                                    (error
                                         "accessing a point that doesn't exist!")
                                    mGeoIndex
    in Map.insert p ((geoIndex + depth) `mod` magicModulo) erosionLevels

answer1 :: Parsed1 -> Int
answer1 d@(depth, (tx, ty)) =
    sum $
    fmap (`mod` 3) $
    foldl (flip (insertErosionLevel d)) mempty $ liftA2 (,) [0 .. tx] [0 .. ty]

parse2 :: String -> Parsed1
--parse2 _ = (510, (10, 10))
parse2 _ = (11817, (9, 751))

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
                        (\(thisPath, (_, pathCost)) ->
                             case Map.lookup (head thisPath) seen of
                                 Nothing -> True
                                 Just lowestCost -> pathCost < lowestCost) $
                    fmap
                        (\(p, (expectedRemaining, stepCost)) ->
                             ( p : ps
                             , ( expectedRemaining <> stepCost <> prevCost
                               , stepCost <> prevCost))) $
                    getOptions $ head ps
            let (done, inProgress) =
                    partition
                        (\(_, (totalExpectedCost, pathCost)) ->
                             totalExpectedCost == pathCost)
                        nextPossible
            let shortestKnownPath' =
                    case sortOn snd done of
                        ((psNew, (_, cNew)):_) ->
                            case shortestKnownPath of
                                Nothing -> Just (cNew, psNew)
                                Just (cOld, psOld) ->
                                    if cNew < cOld
                                        then Just (cNew, psNew)
                                        else Just (cOld, psOld)
                        [] -> shortestKnownPath
            let newSearchNodes =
                    filter
                        (\(_, (totalExpectedCost, _))
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
                        (fmap (\(h:_, (_, c)) -> (h, c)) newSearchNodes) -- should this be nextPossible?
                    -- note that maps don't merge values with <>, they merge with const
                    -- we take advantage of this here, since we already filtered out anything
                    -- more costly than what was already there.
                     <>
                    seen
            let queue'' =
                    queue' <>
                    Set.fromList (fmap (\(ps, c) -> (c, ps)) newSearchNodes)
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

data Equipment
    = ClimbingGear
    | Torch
    | Neither -- Don't want to deal with Maybes, even though that's normally how I'd do it
    deriving (Eq, Ord, Show)

data Terrain
    = Rocky
    | Wet
    | Narrow
    deriving (Show)

isEquipedFor :: Equipment -> Terrain -> Bool
isEquipedFor Neither Rocky = False
isEquipedFor Torch Wet = False
isEquipedFor ClimbingGear Narrow = False
isEquipedFor _ _ = True

commonEquipment :: Terrain -> Terrain -> [Equipment]
commonEquipment a b =
    filter
        (\e -> isEquipedFor e a && isEquipedFor e b)
        [ClimbingGear, Torch, Neither]

getSurroundingPoints :: (Int, Int) -> [(Int, Int)]
getSurroundingPoints (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

caveClimb ::
       Map (Int, Int) Terrain
    -> (Int, Int)
    -> (Equipment, (Int, Int))
    -> [((Equipment, (Int, Int)), (Sum Int, Sum Int))]
caveClimb terrainMap target (currentEquipment, pInit) =
    let currentTerrain = fromJust $ Map.lookup pInit terrainMap
    in fmap
           (\(t, p) ->
                if p == target && currentEquipment /= Torch
                    then ((Torch, p), (Sum $ dist p target, 8))
                    else if isEquipedFor currentEquipment t
                             then ( (currentEquipment, p)
                                  , (Sum $ dist p target, 1))
                             else ( (head $ commonEquipment currentTerrain t, p)
                                  , (Sum $ dist p target, 8))) $
       -- can't move into any point if you can't equip appropriate gear
       filter (not . null . commonEquipment currentTerrain . fst) $
       fmap
           (\p ->
                ( fromMaybe
                      (error $
                       "Error: Map isn't big enough!  Tried to access " ++
                       show p)
                      (Map.lookup p terrainMap)
                , p)) $
       filter (\(x, y) -> x >= 0 && y >= 0) $ getSurroundingPoints pInit

erosionLevelToTerrain :: Int -> Terrain
erosionLevelToTerrain x =
    case x `mod` 3 of
        0 -> Rocky
        1 -> Wet
        2 -> Narrow

answer2 :: Parsed1 -> Maybe (Sum Int, [(Equipment, (Int, Int))])
answer2 d@(depth, target@(tx, ty)) =
    let terrainMap =
            fmap erosionLevelToTerrain $
            foldl (flip (insertErosionLevel d)) mempty $
            -- lazy here, just kept expanding the map until it worked
            liftA2 (,) [0 .. tx + 500] [0 .. ty + 500]
    in aStar (caveClimb terrainMap target) (Torch, (0, 0))
