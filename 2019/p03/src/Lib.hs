module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , parseWire
    , wirePoints
    , wirePoints2
    , stepsToPoint
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
import AdventOfCode.Util (Vector(..), elmTrace, manLen)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import Data.List (foldl', sort, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
       (findIndexL, fromList, reverse)
import Data.Set (Set)
import qualified Data.Set as Set
       (delete, fromList, intersection, toList, union)

data Dir
    = U Integer
    | D Integer
    | L Integer
    | R Integer
    deriving (Show, Eq, Ord)

parseDir :: String -> Dir
parseDir ('U':t) = U (read t)
parseDir ('D':t) = D (read t)
parseDir ('L':t) = L (read t)
parseDir ('R':t) = R (read t)
parseDir _ = error "Dead"

parseWire :: String -> [Dir]
parseWire = fmap parseDir . splitOn ","

parse1 :: String -> ([Dir], [Dir])
parse1 x =
    let (a:b:_) = fmap parseWire $ lines x
    in (a, b)

parse2 :: String -> _
parse2 = parse1

dirToPoints :: (Integer, Integer) -> Dir -> [(Integer, Integer)]
dirToPoints init (U x) =
    reverse $ fmap (\y -> getVector $ Vector init <> Vector (0, y)) [1 .. x]
dirToPoints init (D x) =
    reverse $ fmap (\y -> getVector $ Vector init <> Vector (0, -y)) [1 .. x]
dirToPoints init (L x) =
    reverse $ fmap (\y -> getVector $ Vector init <> Vector (-y, 0)) [1 .. x]
dirToPoints init (R x) =
    reverse $ fmap (\y -> getVector $ Vector init <> Vector (y, 0)) [1 .. x]

wirePoints :: [Dir] -> Set (Integer, Integer)
wirePoints =
    snd .
    foldl'
        (\(point, set) dir ->
             let newPoints = dirToPoints point dir
             in (head newPoints, set `Set.union` Set.fromList newPoints))
        ((0, 0), mempty)

-- Manually did manhattan distance at the end
answer1 :: _ -> _
answer1 (a, b) =
    let wA = wirePoints a
        wB = wirePoints b
        overlap = wA `Set.intersection` wB
    in head $ drop 1 $ sortOn (\x -> manLen $ Vector x) $ Set.toList overlap

wirePoints2 :: [Dir] -> Seq (Integer, Integer)
wirePoints2 =
    Seq.reverse .
    snd .
    foldl'
        (\(point, seq) dir ->
             let newPoints = dirToPoints point dir
             in (head newPoints, Seq.fromList newPoints >< seq))
        ((0, 0), mempty)

stepsToPoint :: (Integer, Integer) -> Seq (Integer, Integer) -> Int
stepsToPoint p seq =
    case Seq.findIndexL (\x -> x == p) seq of
        Just x -> x
        Nothing -> error "damn!"

-- Didn't account for starting step, so just manually added 2 to the result
answer2 :: _ -> _
answer2 (a, b) =
    let wA = wirePoints2 a
        wB = wirePoints2 b
        overlap =
            Set.delete (0, 0) $
            Set.fromList (toList wA) `Set.intersection` Set.fromList (toList wB)
    in head $
       sort . fmap (\x -> stepsToPoint x wA + stepsToPoint x wB) $
       Set.toList overlap
