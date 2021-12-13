{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
--
--  - [Parsing](#g:1)
--  - [Printing](#g:2)
--  - [Hashing](#g:3)
--  - [Cycles](#g:4)
--  - [Iteration](#g:5)
--  - [Path Finding](#g:6)
--  - [Geometry](#g:7)
--  - [Testing](#g:8)
--  - [Misc](#g:9)
module AdventOfCode.Util
    ( labelTrace
    , trace
    , traceShow
      -- * Parsing
    , multiLines
    , parseGrid
    , runPA
      -- * Printing
    , byteStringToHex
    , prettyPrintPointMap
    , prettyPrintPointMapFlippable
    , prettyPrintPointSet
    , prettyPrintPointSetFlippable
      -- * Hashing
    , xmasHash
      -- * Cycles
    , findCyclePeriod
    , findCycle
      -- * Iteration
    , applyNTimes
    , asCounted
    , boundedUntilWithCount
    , boundedUntil
      -- * Path Finding
    , AStarStepOption2(..)
    , aStar2
    , explore
    , explorePaths
      -- * Geometry
    , Vector(..)
    , intoVector
    , manLen
    , R2
    , left
    , rotate
    , upgrade
    , BoundingBox(..)
    , intoBoundingBox
    , isBoundedBy
    , area
    -- * Misc
    , listToIndexMap
    -- * Testing
    , autoFileTest
    -- * Deprecated
    , manDist
    , Manhattan
    , AStarStepOption(..)
    , aStar
    , elmTrace
    ) where

import Control.Applicative (liftA2)
import Control.Exception (catch, throw)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State.Lazy (State, evalState, get, put)
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString, unpack)
import Data.Group (Group (..), Abelian)
import Data.List (foldl', partition, sortOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Ratio (Ratio)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace (trace, traceShow)
import GHC.Word (Word8)
import System.IO.Error (IOError, isDoesNotExistError)
import Test.Hspec (pendingWith, shouldBe)
import Text.ParserCombinators.PArrow (MD)
import qualified Text.ParserCombinators.PArrow as PA

-- | This runs a PArrow parser against a string, returning the successful
-- result.  THIS IS IMPURE and will throw an error if the input is not accepted
-- by the parser.  This is useful for AoC, where inputs are always well behaved
-- and never fail.
--
-- >>> runP (sepBy1 (many1 digit >>> arr read) (char ',')) "123,456,789"
-- [123, 456, 789]
runPA :: MD String a -> String -> a
runPA p = either (error . concat) id . PA.runParser p

-- | This tries to act much like the base function 'lines' but for data that
-- uses _two_ newlines to separate grouped data.  So instead of ending up with
-- a list of Strings, you end up with a list of list of strings.
--
-- >>> multiLines "abc\ndef\n\n123\n456"
-- [["abc","def"],["123","456"]]
multiLines :: String -> [[String]]
multiLines = splitOn [""] . lines

-- | "Fold" over a string draws a 2D grid with characters.  This is useful for
-- parsing mazes or games of life or etc.
--
-- >>> parseGrid (\p c -> if c == '#' then Set.insert else id) " # #\n# # \n"
-- Set.fromList [(0,1), (0,3), (1,0), (1,2)]
parseGrid ::
       (Integral a, Num a) => ((a, a) -> Char -> b -> b) -> b -> String -> b
parseGrid f init s =
    let go (x, y) fn r ((h:t):t2) = go (x + 1, y) fn (fn (x, y) h r) (t : t2)
        go (x, y) fn r ([]:t) = go (0, y + 1) fn r t
        go (x, y) fn r [] = r
    in go (0, 0) f init (lines s)

-- | Vector is a general type for getting common behaviors across vectors of
-- arbitary dimensions.
newtype Vector a = Vector
    { getVector :: a
    } deriving (Show, Ord, Eq)

instance Semigroup (Vector Integer) where
    Vector a <> Vector b = Vector (a + b)

instance Monoid (Vector Integer) where
    mempty = Vector 0

instance Group (Vector Integer) where
    invert (Vector x) = (Vector (-x))

instance Abelian (Vector Integer)

instance Semigroup (Vector (Integer, Integer)) where
    Vector (a0, a1) <> Vector (b0, b1) = Vector (a0 + b0, a1 + b1)

instance Semigroup (Vector (Ratio Integer, Ratio Integer)) where
    Vector (a0, a1) <> Vector (b0, b1) = Vector (a0 + b0, a1 + b1)

instance Monoid (Vector (Integer, Integer)) where
    mempty = Vector (0, 0)

instance Monoid (Vector (Ratio Integer, Ratio Integer)) where
    mempty = Vector (0, 0)

instance Group (Vector (Integer, Integer)) where
    invert (Vector (a, b)) = Vector (-a, -b)

instance Group (Vector (Ratio Integer, Ratio Integer)) where
    invert (Vector (a, b)) = Vector (-a, -b)

instance Abelian (Vector (Integer, Integer))

instance Abelian (Vector (Ratio Integer, Ratio Integer))

instance Semigroup (Vector (Integer, Integer, Integer)) where
    Vector (a0, a1, a2) <> Vector (b0, b1, b2) =
        Vector (a0 + b0, a1 + b1, a2 + b2)

instance Monoid (Vector (Integer, Integer, Integer)) where
    mempty = Vector (0, 0, 0)

instance Group (Vector (Integer, Integer, Integer)) where
    invert (Vector (a, b, c)) = Vector (-a, -b, -c)

instance Abelian (Vector (Integer, Integer, Integer))

instance Semigroup (Vector (Integer, Integer, Integer, Integer)) where
    Vector (a0, a1, a2, a3) <> Vector (b0, b1, b2, b3) =
        Vector (a0 + b0, a1 + b1, a2 + b2, a3 + b3)

instance Monoid (Vector (Integer, Integer, Integer, Integer)) where
    mempty = Vector (0, 0, 0, 0)

instance Group (Vector (Integer, Integer, Integer, Integer)) where
    invert (Vector (a, b, c, d)) = Vector (-a, -b, -c, -d)

instance Abelian (Vector (Integer, Integer, Integer, Integer))

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
            p = min pa pb
        in BoundingBox
           {point = p, vector = Vector $ max (pa + va) (pb + vb) - p}

instance Semigroup (BoundingBox (Integer, Integer)) where
    a <> b =
        let BoundingBox {point = (pa0, pa1), vector = Vector (va0, va1)} = a
            BoundingBox {point = (pb0, pb1), vector = Vector (vb0, vb1)} = b
            p0 = min pa0 pb0
            p1 = min pa1 pb1
        in BoundingBox
           { point = (p0, p1)
           , vector =
                 Vector
                     ( max (pa0 + va0) (pb0 + vb0) - p0
                     , max (pa1 + va1) (pb1 + vb1) - p1)
           }

instance Semigroup (BoundingBox (Integer, Integer, Integer)) where
    a <> b =
        let BoundingBox { point = (pa0, pa1, pa2)
                        , vector = Vector (va0, va1, va2)
                        } = a
            BoundingBox { point = (pb0, pb1, pb2)
                        , vector = Vector (vb0, vb1, vb2)
                        } = b
            p0 = min pa0 pb0
            p1 = min pa1 pb1
            p2 = min pa2 pb2
        in BoundingBox
           { point = (p0, p1, p2)
           , vector =
                 Vector
                     ( max (pa0 + va0) (pb0 + vb0) - p0
                     , max (pa1 + va1) (pb1 + vb1) - p1
                     , max (pa2 + va2) (pb2 + vb2) - p2)
           }

instance Semigroup (BoundingBox (Integer, Integer, Integer, Integer)) where
    a <> b =
        let BoundingBox { point = (pa0, pa1, pa2, pa3)
                        , vector = Vector (va0, va1, va2, va3)
                        } = a
            BoundingBox { point = (pb0, pb1, pb2, pb3)
                        , vector = Vector (vb0, vb1, vb2, vb3)
                        } = b
            p0 = min pa0 pb0
            p1 = min pa1 pb1
            p2 = min pa2 pb2
            p3 = min pa3 pb3
        in BoundingBox
           { point = (p0, p1, p2, p3)
           , vector =
                 Vector
                     ( max (pa0 + va0) (pb0 + vb0) - p0
                     , max (pa1 + va1) (pb1 + vb1) - p1
                     , max (pa2 + va2) (pb2 + vb2) - p2
                     , max (pa3 + va3) (pb3 + vb3) - p3)
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

data R2 a =
    R2 a -- Upper Left
       a -- Upper Right
       a -- Bottom Left
       a -- Bottom Right
    deriving (Show, Eq)

rotate :: Num a => R2 a -> Vector (a, a) -> Vector (a, a)
rotate (R2 ul ur bl br) (Vector (x, y)) =
    Vector (x * ul + y * ur, x * bl + y * br)

left :: Num a => R2 a
left = R2 0 (-1) 1 0

instance Num a => Semigroup (R2 a) where
    R2 ul1 ur1 bl1 br1 <> R2 ul2 ur2 bl2 br2 =
        R2
            (ul1 * ul2 + ur1 * bl2)
            (ul1 * ur2 + ur1 * br2)
            (bl1 * ul2 + br1 * bl2)
            (bl1 * ur2 + br1 * br2)

instance Num a => Monoid (R2 a) where
    mempty = R2 1 0 0 1

upgrade :: Num a => Vector a -> R2 a -> Vector (a, a)
upgrade (Vector x) r = rotate r (Vector (x, 0))

prettyPrintPointMapFlippable ::
       Integral b => Bool -> Char -> (a -> Char) -> Map (b, b) a -> String
prettyPrintPointMapFlippable flipped d fn m =
    let (xs, ys) = unzip $ Map.keys m
        (minX, minY) = (minimum xs, minimum ys)
        (maxX, maxY) = (maximum xs, maximum ys)
    in unlines $
       chunksOf (fromIntegral maxX - fromIntegral minX + 1) $
       fmap (\(y, x) -> fromMaybe d $ fn <$> Map.lookup (x, y) m) $
       liftA2
           (,)
           ((if flipped
                 then id
                 else reverse)
                [minY .. maxY])
           [minX .. maxX]

prettyPrintPointMap ::
       Integral b => Char -> (a -> Char) -> Map (b, b) a -> String
prettyPrintPointMap = prettyPrintPointMapFlippable False

prettyPrintPointSetFlippable ::
       Integral b => Bool -> Char -> Char -> Set (b, b) -> String
prettyPrintPointSetFlippable flipped notPresent present =
    prettyPrintPointMapFlippable flipped notPresent (const present) .
    Map.fromList . fmap (\x -> (x, ())) . Set.toList

prettyPrintPointSet :: Integral b => Char -> Char -> Set (b, b) -> String
prettyPrintPointSet = prettyPrintPointSetFlippable False

word4in8ToHex :: Word8 -> Char
word4in8ToHex 0 = '0'
word4in8ToHex 1 = '1'
word4in8ToHex 2 = '2'
word4in8ToHex 3 = '3'
word4in8ToHex 4 = '4'
word4in8ToHex 5 = '5'
word4in8ToHex 6 = '6'
word4in8ToHex 7 = '7'
word4in8ToHex 8 = '8'
word4in8ToHex 9 = '9'
word4in8ToHex 10 = 'a'
word4in8ToHex 11 = 'b'
word4in8ToHex 12 = 'c'
word4in8ToHex 13 = 'd'
word4in8ToHex 14 = 'e'
word4in8ToHex 15 = 'f'
word4in8ToHex _ = error "Tried to convert value too large to hex!"

word8ToHex :: Word8 -> String
word8ToHex x =
    let (top, bottom) = x `divMod` 16
    in [word4in8ToHex top, word4in8ToHex bottom]

byteStringToHex :: ByteString -> String
byteStringToHex = concatMap word8ToHex . unpack

-- I just never rememeber this syntax ><
-- | Repeatedly apply a function its result n times
--
-- >>> applyNTimes ((+) 2) 0 5
-- 10
applyNTimes :: (a -> a) -> a -> Integer -> a
applyNTimes fn init n = iterate fn init !! fromIntegral n

-- | Take a function, and then have it count its iterations
--
-- >>> asCounted ((+) 2) (0, 4)
-- (1, 6)
asCounted :: (a -> a) -> ((Integer, a) -> (Integer, a))
asCounted fn (i, a) = (i + 1, fn a)

-- | Run a function repeatedly until a predicate is met, but throw an error if
-- more than a specified number of iterations is run.  Also, the count is
-- returned.
--
-- >>> boundedUntilWithCount 1000 (> 10) ((+) 2) 0
-- (5, 12)
boundedUntilWithCount :: Integer -> (a -> Bool) -> (a -> a) -> a -> (Integer, a)
boundedUntilWithCount max pred fn x =
    if max <= 0
        then error "Iteration bounds was set at or below 0!"
        else let (iterations, last) =
                     until (\(i, y) -> i >= max || pred y) (asCounted fn) (0, x)
             in if iterations == max && not (pred last)
                    then error "Too many iterations!"
                    else (iterations, last)

-- | Run a function repeatedly until a predicate is met, but throw an error if
-- more than a specified nnumber of iterations is run.
--
-- >>> boundedUntil 1000 (> 10) ((+) 2) 0
-- 12
boundedUntil :: Integer -> (a -> Bool) -> (a -> a) -> a -> a
boundedUntil max pred fn = snd . boundedUntilWithCount max pred fn

-- | Find the period of a cycle (Brent's Algorithm pt1)
findCyclePeriod :: Eq a => (a -> a) -> a -> Integer
findCyclePeriod fn init = go 1 1 (fn init) fn init
  where
    go !pow !guess !hare !fn !tort =
        if tort == hare
            then guess
            else if pow == guess
                     then go (pow * 2) 1 (fn hare) fn hare
                     else go pow (guess + 1) (fn hare) fn tort

-- | Find the start, period, first value of a cycle (Brent's Algorithm pt1/2)
findCycle :: Eq a => (a -> a) -> a -> (Integer, Integer, a)
findCycle fn init =
    let period = findCyclePeriod fn init
        hare = applyNTimes fn init period
        (firstIndex, (firstValue, _)) =
            until
                (uncurry (==) . snd)
                (asCounted (\(a, b) -> (fn a, fn b)))
                (0, (init, hare))
    in (firstIndex, period, firstValue)

-- | Note: this isn't actually how elm's debug works
elmTrace :: Show a => a -> a
elmTrace x = traceShow x x

-- | Print out both a label and the item being traced.  This is notably similar
-- to how elm's debug works, it's just that `elmTrace` accidentally took the
-- name with an incorrect impl.
--
-- >>> labelTrace "tag" 45
-- ("tag", 45)
-- 45
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
    } deriving (Show)

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
                        (\AStarStepOption { position
                                          , stepCost
                                          , minimumRemainingCost
                                          } ->
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
        -- If we start in the solved state, there's no way to notice this, and
        -- the result will be wrong.  Use aStar2.
        (Nothing, Map.singleton pInit mempty, Set.singleton (mempty, pInit, []))

-- | Generic representation of possible step we could take when exploring a
-- discretely divided space
data AStarStepOption2 a b = AStarStepOption2
    { position :: a -- ^ The new position, if the option is taken
    , stepCost :: b -- ^ How much it costs to accept this option
    } deriving (Show)

-- Ideally we would implement aStarStep in terms of this impl, but we can't upgrade, only downgrade.  We keep around the others for compatibility.  It would be nice to just publish this as a package so that it could be version managed.
-- | The A* algorithm.  This is essentially a depth first search with a
-- priority queue based on a huristic for guessing the lowest cost to win.
--
-- This particular approach is highly generic, allowing almost anything to
-- operate as a position or a cost.
--
-- >>> aStar2 (\p -> abs (p - 3)) (\p -> [AStarStepOption2 (p-1) 1, AStarStepOption2 (p+1) 1]) 0
-- Just (3, [0,1,2,3])
--
-- In this example above, we are simply solving walking a number line from 0
-- towards a goal state of 3.  We never explore the negative direction, because
-- it always looks unattractive (and our hueristic for a lower bound can be
-- optimal here).
--
-- For /longest/ path finding:
--
--   - invert your cost (ex. 0 - cost)
--   - set goal function to -Inf when not at goal state and 0 when at a goal state (unless you can come up with a tighter worst case bound)
aStar2 ::
       (Ord cost, Ord position, Monoid cost)
    => (position -> cost) -- ^ Compute the lowest possible known cost to arrive at a goal state from any other position.  All goal states should simply return mempty (typically 0).  Ideally, this is given as tightly as possible, this cost is ever _over_ estimated, the algorithm will be wrong.
    -> (position -> [AStarStepOption2 position cost]) -- ^ All possible steps one could take from a given position
    -> position -- ^ The starting position
    -> Maybe (cost, [position]) -- ^ If successful, the lowest possible cost and (one of) the lowest costs path(s) to get there
aStar2 getLowerBoundCost getOptions pInit =
    fromJust $
    evalState
        (iterateWhile
             isNothing
             (aStarStep
                  (fmap
                       (\AStarStepOption2 {position, stepCost} ->
                            AStarStepOption
                                position
                                stepCost
                                (getLowerBoundCost position)) .
                   getOptions)))
        ( if getLowerBoundCost pInit == mempty
              then Just (mempty, [pInit])
              else Nothing
        , Map.singleton pInit mempty
        , Set.singleton (mempty, pInit, []))

exploreStep ::
       (Ord cost, Monoid cost, Ord position)
    => cost
    -> (position -> [AStarStepOption2 position cost])
    -> State (Map position cost, Set (cost, position)) (Maybe (Map position cost))
exploreStep maxCost getOptions = do
    (seen, queue) <- get
    case Set.minView queue of
        Nothing -> return $ Just seen -- out of explorable points
        Just ((prevCost, cheapestGuessPosition), queue') -> do
            let newSearchNodes =
                    filter
                        (\(pathCost, position) ->
                             pathCost <= maxCost &&
                             case Map.lookup position seen of
                                 Nothing -> True
                                 -- TODO: We'd ideally like to remove this previous entry from the queue
                                 Just lowestCost -> pathCost < lowestCost) $
                    fmap
                        (\AStarStepOption2 {position, stepCost} ->
                             (stepCost <> prevCost, position)) $
                    getOptions cheapestGuessPosition
            put $
                foldl'
                    (\(s, q) x -> (Map.insert (snd x) (fst x) s, Set.insert x q))
                    (seen, queue')
                    newSearchNodes
            return Nothing

-- | Find all possible paths, with or without (a) goal state(s)
explore ::
       (Ord cost, Ord position, Monoid cost)
    => cost -- ^ The maximum cost you're willing to consider
    -> (position -> [AStarStepOption2 position cost]) -- ^ Next step function (same as `aStar2`).  If a goal state is reached this should return [] or certain explorations will walk indefinitely.
    -> position -- ^ Starting position
    -> Map position cost -- ^ All positions discovered + the minimum cost to achieve the position from the starting state
explore maxCost getOptions pInit =
    fromJust $
    evalState
        (iterateWhile isNothing (exploreStep maxCost getOptions))
        (Map.singleton pInit mempty, Set.singleton (mempty, pInit))

-- | Find every possible path (lazily).  This is the starting point for a DFS.
--
-- Be sure that options do eventually terminate or this will run infinitely.
-- Notably, position can be abstract, to consider state as one explores a
-- geometric space.  Unlike `explore` and `aStar2` this omits a cost
-- consideration, but it can be included by considering the cost a part of the
-- position (as there is never any attempt to prune).
--
-- >>> take 3 $ explorePaths (\p -> if p >= 5 then [] else [p+1, p+2]) 0
-- [[0,1,2,3,4,5],[0,1,2,3,4,6],[0,1,2,3,5]]
explorePaths ::
  -- | Function to get all next possible positions based on the current
  (position -> [position]) ->
  -- | The starting position
  position ->
  -- | The list of all paths (which are lists of positions)
  [[position]]
explorePaths f p =
  case f p of
    [] -> [[p]]
    os -> ((:) p) <$> (explorePaths f =<< os)

-- | Common hashing approach in 2016 problems where we want the hex
-- representation as a string out.
--
-- >>> xmasHash "A string!"
-- "a429311125de36d0beb338ab6d509404"
xmasHash :: String -> String
xmasHash = byteStringToHex . MD5.hash . encodeUtf8 . pack

-- | Create a test that is enabled only if the expected file with the input
-- string is present
--
-- >>> hspec $ it "auto skipped" $ autoFileTest id "./input.txt" "the contents of the file"
-- auto skipped
--   # PENDING: File ./input.txt does not exist
autoFileTest f fileName expected = do
    mContents <-
        (Just <$> readFile fileName) `catch`
        (\e ->
             if isDoesNotExistError e
                 then return Nothing
                 else throw (e :: IOError))
    case mContents of
        Nothing -> pendingWith ("File " <> fileName <> " does not exist")
        Just contents -> f contents `shouldBe` expected
