module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import AdventOfCode.Util
       (applyNTimes, elmTrace, findCycle, parseGrid)
import Control.Applicative (liftA2)
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Monoid ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
       --(Integral a, Num a) => ((a, a) -> Char -> b -> b) -> b -> String -> b
       --

data BugIsh
    = Bug
    | Empty
    deriving (Show, Eq, Ord)

parse1 :: String -> Map (Int, Int) BugIsh
parse1 =
    parseGrid
        (\p c m ->
             case c of
                 '#' -> Map.insert p Bug m
                 '.' -> Map.insert p Empty m
                 _ -> m)
        mempty

parse2 :: String -> _
parse2 = parse1

step :: Map (Int, Int) BugIsh -> Map (Int, Int) BugIsh
step start =
    Map.foldrWithKey
        (\(x, y) b m ->
             let consider = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
                 count =
                     length $
                     filter
                         (\p -> fromMaybe Empty (Map.lookup p start) == Bug)
                         consider
             in case b of
                    Bug ->
                        if count == 1
                            then Map.insert (x, y) Bug m
                            else Map.insert (x, y) Empty m
                    Empty ->
                        if count == 1 || count == 2
                            then Map.insert (x, y) Bug m
                            else Map.insert (x, y) Empty m)
        mempty
        start

biodiversity :: Map (Int, Int) BugIsh -> Int
biodiversity =
    Map.foldrWithKey
        (\(x, y) b r ->
             case b of
                 Empty -> r
                 Bug -> r + 2 ^ (x + y * 5))
        0

--findCycle :: Eq a => (a -> a) -> a -> (Integer, Integer, a)
answer1 :: _ -> _
answer1 = biodiversity . (\(_, _, c) -> c) . findCycle step

pointsToQuery :: (Int, (Int, Int)) -> [(Int, (Int, Int))]
pointsToQuery (z, (2, 1)) =
    [ (z, (2, 0))
    , (z, (1, 1))
    , (z, (3, 1))
    , (z - 1, (0, 0))
    , (z - 1, (1, 0))
    , (z - 1, (2, 0))
    , (z - 1, (3, 0))
    , (z - 1, (4, 0))
    ]
pointsToQuery (z, (1, 2)) =
    [ (z, (1, 3))
    , (z, (1, 1))
    , (z, (0, 2))
    , (z - 1, (0, 0))
    , (z - 1, (0, 1))
    , (z - 1, (0, 2))
    , (z - 1, (0, 3))
    , (z - 1, (0, 4))
    ]
pointsToQuery (z, (3, 2)) =
    [ (z, (3, 1))
    , (z, (3, 3))
    , (z, (4, 2))
    , (z - 1, (4, 0))
    , (z - 1, (4, 1))
    , (z - 1, (4, 2))
    , (z - 1, (4, 3))
    , (z - 1, (4, 4))
    ]
pointsToQuery (z, (2, 3)) =
    [ (z, (2, 4))
    , (z, (1, 3))
    , (z, (3, 3))
    , (z - 1, (0, 4))
    , (z - 1, (1, 4))
    , (z - 1, (2, 4))
    , (z - 1, (3, 4))
    , (z - 1, (4, 4))
    ]
pointsToQuery (z, (0, 0)) =
    [(z, (0, 1)), (z, (1, 0)), (z + 1, (2, 1)), (z + 1, (1, 2))]
pointsToQuery (z, (4, 0)) =
    [(z, (4, 1)), (z, (3, 0)), (z + 1, (2, 1)), (z + 1, (3, 2))]
pointsToQuery (z, (0, 4)) =
    [(z, (1, 4)), (z, (0, 3)), (z + 1, (1, 2)), (z + 1, (2, 3))]
pointsToQuery (z, (4, 4)) =
    [(z, (4, 3)), (z, (3, 4)), (z + 1, (3, 2)), (z + 1, (2, 3))]
pointsToQuery (z, (x, 0)) =
    [(z, (x + 1, 0)), (z, (x - 1, 0)), (z, (x, 1)), (z + 1, (2, 1))]
pointsToQuery (z, (x, 4)) =
    [(z, (x + 1, 4)), (z, (x - 1, 4)), (z, (x, 3)), (z + 1, (2, 3))]
pointsToQuery (z, (0, y)) =
    [(z, (0, y + 1)), (z, (0, y - 1)), (z, (1, y)), (z + 1, (1, 2))]
pointsToQuery (z, (4, y)) =
    [(z, (4, y + 1)), (z, (4, y - 1)), (z, (3, y)), (z + 1, (3, 2))]
pointsToQuery (z, (x, y)) =
    [(z, (x + 1, y)), (z, (x - 1, y)), (z, (x, y + 1)), (z, (x, y - 1))]

minute :: Set (Int, (Int, Int)) -> Set (Int, (Int, Int))
minute start =
    let poi =
            Set.toList $
            Set.delete (2, 2) $ Set.fromList $ liftA2 (,) [0 .. 4] [0 .. 4]
        lowestLevel = fst $ fromJust $ Set.lookupMin start
        highestLevel = fst $ fromJust $ Set.lookupMax start
        ofInterest = liftA2 (,) [lowestLevel - 1 .. highestLevel + 1] poi
    in foldr
           (\p s ->
                let count =
                        length $
                        filter (\p -> Set.member p start) $ pointsToQuery p
                in if Set.member p start
                       then if count == 1
                                then Set.insert p s
                                else s
                       else if count == 1 || count == 2
                                then Set.insert p s
                                else s)
           mempty
           ofInterest

mapToSet :: Map (Int, Int) BugIsh -> Set (Int, (Int, Int))
mapToSet =
    Map.foldrWithKey
        (\k v s ->
             if v == Bug
                 then Set.insert (0, k) s
                 else s)
        mempty

--applyNTimes :: (a -> a) -> a -> Integer -> a
answer2 :: _ -> _
answer2 m = Set.size $ applyNTimes minute (mapToSet m) 200
