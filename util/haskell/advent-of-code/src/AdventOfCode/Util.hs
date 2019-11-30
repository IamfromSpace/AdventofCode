{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module AdventOfCode.Util
    ( aStar
    , AStarStepOption(..)
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
    , Vector(..)
    , manLen
    , intoVector
    , BoundingBox(..)
    , intoBoundingBox
    , isBoundedBy
    , area
    ) where

--TODO: Tortise and hair cycle dectection
--TODO: Repetition detection
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.List (partition, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow)

newtype Vector a = Vector
    { getVector :: a
    } deriving (Show, Ord, Eq)

instance Semigroup (Vector Integer) where
    Vector a <> Vector b = Vector (a + b)

instance Monoid (Vector Integer) where
    mempty = Vector 0

instance Semigroup (Vector (Integer, Integer)) where
    Vector (a0, a1) <> Vector (b0, b1) = Vector (a0 + b0, a1 + b1)

instance Monoid (Vector (Integer, Integer)) where
    mempty = Vector (0, 0)

instance Semigroup (Vector (Integer, Integer, Integer)) where
    Vector (a0, a1, a2) <> Vector (b0, b1, b2) =
        Vector (a0 + b0, a1 + b1, a2 + b2)

instance Monoid (Vector (Integer, Integer, Integer)) where
    mempty = Vector (0, 0, 0)

instance Semigroup (Vector (Integer, Integer, Integer, Integer)) where
    Vector (a0, a1, a2, a3) <> Vector (b0, b1, b2, b3) =
        Vector (a0 + b0, a1 + b1, a2 + b2, a3 + b3)

instance Monoid (Vector (Integer, Integer, Integer, Integer)) where
    mempty = Vector (0, 0, 0, 0)

class ManhattanLength a where
    manLen :: a -> Integer

instance ManhattanLength (Vector Integer) where
    manLen (Vector x) = abs x

instance ManhattanLength (Vector (Integer, Integer)) where
    manLen (Vector (x, y)) = abs x + abs y

instance ManhattanLength (Vector (Integer, Integer, Integer)) where
    manLen (Vector (x, y, z)) = abs x + abs y + abs z

instance ManhattanLength (Vector (Integer, Integer, Integer, Integer)) where
    manLen (Vector (x, y, z, t)) = abs x + abs y + abs z + abs t

class IntoVector a where
    intoVector :: a -> a -> Vector a

instance IntoVector Integer where
    intoVector a b = Vector (b - a)

instance IntoVector (Integer, Integer) where
    (a0, a1) `intoVector` (b0, b1) = Vector (b0 - a0, b1 - a1)

instance IntoVector (Integer, Integer, Integer) where
    (a0, a1, a2) `intoVector` (b0, b1, b2) = Vector (b0 - a0, b1 - a1, b2 - a2)

instance IntoVector (Integer, Integer, Integer, Integer) where
    (a0, a1, a2, a3) `intoVector` (b0, b1, b2, b3) =
        Vector (b0 - a0, b1 - a1, b2 - a2, b3 - a3)

data BoundingBox a = BoundingBox
    { point :: a
    , vector :: Vector a
    } deriving (Show, Ord, Eq)

class IntoBoundingBox a where
    intoBoundingBox :: a -> BoundingBox a

instance IntoBoundingBox Integer where
    intoBoundingBox p = BoundingBox {point = p, vector = mempty}

instance IntoBoundingBox (Integer, Integer) where
    intoBoundingBox p = BoundingBox {point = p, vector = mempty}

instance IntoBoundingBox (Integer, Integer, Integer) where
    intoBoundingBox p = BoundingBox {point = p, vector = mempty}

instance IntoBoundingBox (Integer, Integer, Integer, Integer) where
    intoBoundingBox p = BoundingBox {point = p, vector = mempty}

instance Semigroup (BoundingBox Integer) where
    a <> b =
        let BoundingBox {point = pa, vector = Vector va} = a
            BoundingBox {point = pb, vector = Vector vb} = b
        in BoundingBox
           {point = min pa pb, vector = Vector $ max (pa + va) (pb + vb)}

instance Semigroup (BoundingBox (Integer, Integer)) where
    a <> b =
        let BoundingBox {point = (pa0, pa1), vector = Vector (va0, va1)} = a
            BoundingBox {point = (pb0, pb1), vector = Vector (vb0, vb1)} = b
        in BoundingBox
           { point = (min pa0 pb0, min pa1 pb1)
           , vector =
                 Vector
                     (max (pa0 + va0) (pb0 + vb0), max (pa1 + va1) (pb1 + vb1))
           }

instance Semigroup (BoundingBox (Integer, Integer, Integer)) where
    a <> b =
        let BoundingBox { point = (pa0, pa1, pa2)
                        , vector = Vector (va0, va1, va2)
                        } = a
            BoundingBox { point = (pb0, pb1, pb2)
                        , vector = Vector (vb0, vb1, vb2)
                        } = b
        in BoundingBox
           { point = (min pa0 pb0, min pa1 pb1, min pa2 pb2)
           , vector =
                 Vector
                     ( max (pa0 + va0) (pb0 + vb0)
                     , max (pa1 + va1) (pb1 + vb1)
                     , max (pa2 + va2) (pb2 + vb2))
           }

instance Semigroup (BoundingBox (Integer, Integer, Integer, Integer)) where
    a <> b =
        let BoundingBox { point = (pa0, pa1, pa2, pa3)
                        , vector = Vector (va0, va1, va2, va3)
                        } = a
            BoundingBox { point = (pb0, pb1, pb2, pb3)
                        , vector = Vector (vb0, vb1, vb2, vb3)
                        } = b
        in BoundingBox
           { point = (min pa0 pb0, min pa1 pb1, min pa2 pb2, min pa3 pb3)
           , vector =
                 Vector
                     ( max (pa0 + va0) (pb0 + vb0)
                     , max (pa1 + va1) (pb1 + vb1)
                     , max (pa2 + va2) (pb2 + vb2)
                     , max (pa3 + va3) (pb3 + vb3))
           }

class IsBoundedBy a where
    isBoundedBy :: a -> BoundingBox a -> Bool

instance IsBoundedBy Integer where
    isBoundedBy p bb =
        let BoundingBox {point = bbp, vector = Vector v} = bb
        in p >= bbp && p <= bbp + v

instance IsBoundedBy (Integer, Integer) where
    isBoundedBy (p0, p1) bb =
        let BoundingBox {point = (bbp0, bbp1), vector = Vector (v0, v1)} = bb
        in p0 >= bbp0 && p1 >= bbp1 && p0 <= (bbp0 + v0) && p1 <= (bbp1 + v1)

instance IsBoundedBy (Integer, Integer, Integer) where
    isBoundedBy (p0, p1, p2) bb =
        let BoundingBox { point = (bbp0, bbp1, bbp2)
                        , vector = Vector (v0, v1, v2)
                        } = bb
        in p0 >= bbp0 &&
           p1 >= bbp1 &&
           p2 >= bbp2 &&
           p0 <= (bbp0 + v0) && p1 <= (bbp1 + v1) && p2 <= (bbp2 + v2)

instance IsBoundedBy (Integer, Integer, Integer, Integer) where
    isBoundedBy (p0, p1, p2, p3) bb =
        let BoundingBox { point = (bbp0, bbp1, bbp2, bbp3)
                        , vector = Vector (v0, v1, v2, v3)
                        } = bb
        in p0 >= bbp0 &&
           p1 >= bbp1 &&
           p2 >= bbp2 &&
           p3 >= bbp3 &&
           p0 <= (bbp0 + v0) &&
           p1 <= (bbp1 + v1) && p2 <= (bbp2 + v2) && p3 <= (bbp3 + v3)

class HasArea a where
    area :: a -> Integer

instance HasArea (BoundingBox Integer) where
    area BoundingBox {vector = Vector v} = v

instance HasArea (BoundingBox (Integer, Integer)) where
    area BoundingBox {vector = Vector (v0, v1)} = v0 * v1

instance HasArea (BoundingBox (Integer, Integer, Integer)) where
    area BoundingBox {vector = Vector (v0, v1, v2)} = v0 * v1 * v2

instance HasArea (BoundingBox (Integer, Integer, Integer, Integer)) where
    area BoundingBox {vector = Vector (v0, v1, v2, v3)} = v0 * v1 * v2 * v3

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
    manDist a b = manLen (intoVector a b)

instance Manhattan (Integer, Integer) where
    manDist a b = manLen (intoVector a b)

instance Manhattan (Integer, Integer, Integer) where
    manDist a b = manLen (intoVector a b)

instance Manhattan (Integer, Integer, Integer, Integer) where
    manDist a b = manLen (intoVector a b)

listToIndexMap :: [a] -> Map Int a
listToIndexMap =
    fst . foldl (\(m, i) v -> (Map.insert i v m, i + 1)) (mempty, 0)

mMaybeinimumBy :: Ord b => (a -> b) -> [a] -> Maybe a
mMaybeinimumBy _ [] = Nothing
mMaybeinimumBy fn (h:t) = mMaybeinimumBy' h fn t
  where
    mMaybeinimumBy' :: Ord b => a -> (a -> b) -> [a] -> Maybe a
    mMaybeinimumBy' !result _ [] = Just result
    mMaybeinimumBy' !result !fn (h:t) =
        let newResult =
                if fn h < fn result
                    then h
                    else result
        in mMaybeinimumBy' newResult fn t

data AStarStepOption a b = AStarStepOption
    { position :: a
    , stepCost :: b
    , minimumRemainingCost :: b
    }

aStarStep ::
       (Ord cost, Monoid cost, Ord position)
    => (position -> [AStarStepOption position cost])
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
                        (\(AStarStepOption { position
                                           , stepCost
                                           , minimumRemainingCost
                                           }) ->
                             ( position
                             , cheapestGuessPosition : history
                             , ( minimumRemainingCost <> stepCost <> prevCost
                               , stepCost <> prevCost))) $
                    getOptions cheapestGuessPosition
            let (done, inProgress) =
                    partition
                        (\(_, _, (totalExpectedCost, pathCost)) ->
                             totalExpectedCost == pathCost)
                        nextPossible
            let shortestKnownPath' =
                    case mMaybeinimumBy
                             (\(_, _, expectedThenCurrentCost) ->
                                  expectedThenCurrentCost)
                             done of
                        Just (currentPositionNew, historyNew, (_, pathCostNew)) ->
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
                        Nothing -> shortestKnownPath
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
    => (position -> [AStarStepOption position cost])
    -> position
    -> Maybe (cost, [position])
aStar getOptions pInit =
    fromJust $
    evalState
        (iterateWhile isNothing (aStarStep getOptions))
        (Nothing, mempty, Set.singleton (mempty, pInit, []))
