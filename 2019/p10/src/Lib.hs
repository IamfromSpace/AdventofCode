module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , firingSequence
    , firingSequenceX
    , getSlope
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import AdventOfCode.Util ()
import Control.Applicative (liftA2)
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List (cycle)
import Data.List.Split ()
import Data.Map (Map)
import qualified Data.Map as Map (alter, lookup)
import Data.Maybe ()
import Data.Monoid ()
import Data.Ratio (Ratio, (%))
import Data.Sequence ()
import Data.Set (Set)
import qualified Data.Set as Set
       (delete, foldr, fromList, insert, null, singleton, toList, union)

parseLine :: Set (Int, Int) -> Int -> Int -> String -> Set (Int, Int)
parseLine s _ _ [] = s
parseLine s y x ('.':t) = parseLine s y (x + 1) t
parseLine s y x ('#':t) = parseLine (Set.insert (x, y) s) y (x + 1) t
parseLine s y x ('X':t) = parseLine (Set.insert (x, y) s) y (x + 1) t

parseBlock :: Set (Int, Int) -> Int -> [String] -> Set (Int, Int)
parseBlock s _ [] = s
parseBlock s y (h:t) = parseBlock (parseLine s y 0 h) (y + 1) t

parse1 :: String -> _
parse1 = parseBlock mempty 0 . lines

parse2 :: String -> _
parse2 = parse1

getSlope :: (Int, Int) -> (Int, Int) -> (Maybe (Ratio Int), Bool)
getSlope (x0, y0) (x1, y1) =
    let xDelta = x1 - x0
    in if xDelta == 0
           then (Nothing, y0 > y1)
           else (Just ((y1 - y0) % xDelta), x0 > x1)

allSlopes :: (Int, Int) -> Set (Int, Int) -> Set (Maybe (Ratio Int), Bool)
allSlopes p s = Set.fromList $ fmap (getSlope p) $ Set.toList $ Set.delete p s

answer1 :: _ -> _
answer1 s = maximum $ fmap (length . flip allSlopes s) $ Set.toList s

slopeGroups ::
       (Int, Int)
    -> Set (Int, Int)
    -> Map (Maybe (Ratio Int), Bool) (Set (Int, Int))
slopeGroups p =
    Set.foldr
        (\p2 m ->
             Map.alter
                 (\case
                      Nothing -> Just $ Set.singleton p2
                      Just s -> Just $ Set.insert p2 s)
                 (getSlope p p2)
                 m)
        mempty

firingSequence :: [(Maybe (Ratio Int), Bool)]
firingSequence =
    let base = Set.toList $ Set.fromList $ liftA2 (%) [0 .. 40] [1 .. 40]
    in cycle
           ([(Nothing, True)] ++
            fmap (\x -> (Just (-1 * x), False)) (reverse base) ++
            tail (fmap (\x -> (Just x, False)) base) ++
            [(Nothing, False)] ++
            tail (fmap (\x -> (Just (-1 * x), True)) (reverse base)) ++
            tail (fmap (\x -> (Just x, True)) base))

firingSequenceX :: [(Maybe (Ratio Int), Bool)]
firingSequenceX =
    let base = Set.toList $ Set.fromList $ liftA2 (%) [0 .. 40] [1 .. 40]
    in cycle
           ([(Nothing, False)] ++
            fmap (\x -> (Just x, True)) base ++
            tail (fmap (\x -> (Just (-1 * x), True)) (reverse base)))

fire ::
       (Int, Int)
    -> (Maybe (Ratio Int), Bool)
    -> Map (Maybe (Ratio Int), Bool) (Set (Int, Int))
    -> (Maybe (Int, Int), Map (Maybe (Ratio Int), Bool) (Set (Int, Int)))
fire (x0, y0) angle asts =
    case Map.lookup angle asts of
        Nothing -> (Nothing, asts)
        Just s ->
            let destroyed =
                    snd $
                    minimum $
                    fmap (\p@(x1, y1) -> (abs (y1 - y0) + abs (x1 - x0), p)) $
                    Set.toList s
                nextS = Set.delete destroyed s
                nextM =
                    Map.alter
                        (\_ ->
                             if Set.null nextS
                                 then Nothing
                                 else Just nextS)
                        angle
                        asts
            in (Just destroyed, nextM)

destroyN ::
       (Int, Int)
    -> [(Maybe (Ratio Int), Bool)]
    -> Int
    -> Map (Maybe (Ratio Int), Bool) (Set (Int, Int))
    -> Maybe (Int, Int)
destroyN origin (angle:t) 1 asts =
    let (destroyed, remaining) = fire origin angle asts
    in case destroyed of
           Nothing -> destroyN origin t 1 remaining
           Just p -> Just p
destroyN origin (angle:t) x asts =
    let (destroyed, remaining) = fire origin angle asts
    in case destroyed of
           Nothing -> destroyN origin t x remaining
           Just _ -> destroyN origin t (x - 1) remaining

bestPoint :: Set (Int, Int) -> (Int, Int)
bestPoint s =
    snd $ maximum $ fmap (\x -> (length $ allSlopes x s, x)) $ Set.toList s

answer2 :: _ -> _
answer2 s =
    let origin = bestPoint s
        Just (a, b) =
            destroyN origin firingSequence 200 $
            slopeGroups origin (Set.delete origin s)
    in a * 100 + b
