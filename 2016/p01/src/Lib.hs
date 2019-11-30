module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.Map (Map)
import AdventOfCode.Util (Vector(..), elmTrace)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Data.List (cycle, foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member, union)

type Parsed1 = [Either Integer Integer]

type Parsed2 = Parsed1

data Heading
    = North
    | South
    | East
    | West

parseDir :: String -> Either Integer Integer
parseDir ('R':t) = Right (read t)
parseDir ('L':t) = Left (read t)
parseDir _ = error "dead"

parse1 :: String -> Parsed1
parse1 = fmap parseDir . splitOn ", "

toVec ::
       Heading -> Either Integer Integer -> (Heading, Vector (Integer, Integer))
toVec North (Left x) = (West, Vector (-x, 0))
toVec North (Right x) = (East, Vector (x, 0))
toVec South (Left x) = (East, Vector (x, 0))
toVec South (Right x) = (West, Vector (-x, 0))
toVec East (Left x) = (North, Vector (0, x))
toVec East (Right x) = (South, Vector (0, -x))
toVec West (Left x) = (South, Vector (0, -x))
toVec West (Right x) = (North, Vector (0, x))

parse2 :: String -> Parsed2
parse2 = parse1

totalVector :: Parsed1 -> Vector (Integer, Integer)
totalVector =
    snd .
    foldl'
        (\(heading, v) dir ->
             let (newHeading, v2) = toVec heading dir
             in (newHeading, v <> v2))
        (North, mempty)

vLength :: Vector (Integer, Integer) -> Integer
vLength (Vector (a, b)) = abs a + abs b

answer1 :: Parsed1 -> String
answer1 = show . vLength . totalVector

toVecs ::
       Heading
    -> Either Integer Integer
    -> (Heading, [Vector (Integer, Integer)])
toVecs North (Left x) = (West, fmap (\y -> Vector (y, 0)) [-x .. -1])
toVecs North (Right x) = (East, fmap (\y -> Vector (y, 0)) (reverse [1 .. x]))
toVecs South (Left x) = (East, fmap (\y -> Vector (y, 0)) (reverse [1 .. x]))
toVecs South (Right x) = (West, fmap (\y -> Vector (y, 0)) [-x .. -1])
toVecs East (Left x) = (North, fmap (\y -> Vector (0, y)) (reverse [1 .. x]))
toVecs East (Right x) = (South, fmap (\y -> Vector (0, y)) [-x .. -1])
toVecs West (Left x) = (South, fmap (\y -> Vector (0, y)) [-x .. -1])
toVecs West (Right x) = (North, fmap (\y -> Vector (0, y)) (reverse [1 .. x]))

findFirstMember :: Ord a => [a] -> Set a -> Maybe a
findFirstMember [] s = Nothing
findFirstMember (h:t) s =
    if Set.member h s
        then Just h
        else findFirstMember t s

findReVisit ::
       Set (Vector (Integer, Integer))
    -> Vector (Integer, Integer)
    -> Heading
    -> [Either Integer Integer]
    -> Integer
findReVisit seen position heading (next:t) =
    let (newHeading, vectors) = toVecs heading next
        newVisits = fmap (<> position) (elmTrace vectors)
        newPosition = head newVisits
    in case findFirstMember newVisits seen of
           Just x -> vLength x
           Nothing ->
               findReVisit
                   (Set.fromList newVisits `Set.union` seen)
                   newPosition
                   newHeading
                   t

answer2 :: Parsed2 -> String
answer2 = show . findReVisit mempty mempty North
