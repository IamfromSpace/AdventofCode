module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--Note, this implementation is the cleaned up version after the speed solution!
--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import AdventOfCode.Util (Vector(..), elmTrace, manLen)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import Data.List (foldl', sort, sortOn)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map (alter, lookup)
import Data.Maybe (fromJust)
import Data.Monoid ()
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
       (findIndexL, fromList, reverse)
import Data.Set (Set)
import qualified Data.Set as Set
       (delete, fromList, intersection, toList, union)

parseDir :: String -> [Vector (Integer, Integer)]
parseDir ('U':t) = replicate (read t) (Vector (0, 1))
parseDir ('D':t) = replicate (read t) (Vector (0, -1))
parseDir ('L':t) = replicate (read t) (Vector (-1, 0))
parseDir ('R':t) = replicate (read t) (Vector (1, 0))
parseDir _ = error "Dead"

parseWire :: String -> [Vector (Integer, Integer)]
parseWire = concatMap parseDir . splitOn ","

parse1 :: String -> ([Vector (Integer, Integer)], [Vector (Integer, Integer)])
parse1 x =
    let (a:b:_) = fmap parseWire $ lines x
    in (a, b)

parse2 :: String -> _
parse2 = parse1

stepsToPath :: [Vector (Integer, Integer)] -> [Vector (Integer, Integer)]
stepsToPath =
    uncurry (:) .
    foldl'
        (\(currPoint, path) next -> (currPoint <> next, currPoint : path))
        (Vector (0, 0), [])

answer1 :: _ -> _
answer1 (a, b) =
    let aPath = stepsToPath a
        bPath = stepsToPath b
        overlap =
            Set.delete (Vector (0, 0)) $
            Set.fromList aPath `Set.intersection` Set.fromList bPath
    in minimum $ fmap manLen $ Set.toList overlap

downsert :: Ord k => k -> v -> Map k v -> Map k v
downsert k v =
    Map.alter
        (\case
             Nothing -> Just v
             x -> x)
        k

pointCosts ::
       [Vector (Integer, Integer)] -> Map (Vector (Integer, Integer)) Integer
pointCosts = snd . foldl' (\(i, map) v -> (i + 1, downsert v i map)) (0, mempty)

answer2 :: _ -> _
answer2 (a, b) =
    let aPath = stepsToPath a
        aCosts = pointCosts (reverse aPath)
        bPath = stepsToPath b
        bCosts = pointCosts (reverse bPath)
        overlap =
            Set.delete (Vector (0, 0)) $
            Set.fromList aPath `Set.intersection` Set.fromList bPath
    in minimum .
       fmap
           (\x ->
                fromJust (Map.lookup x aCosts) + fromJust (Map.lookup x bCosts)) $
       Set.toList overlap
