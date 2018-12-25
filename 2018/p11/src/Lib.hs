{-# LANGUAGE BangPatterns #-}

module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , hundredthDigit
    , powerLevel
    ) where

import Control.Applicative (liftA2, liftA3)
import Data.Map
       (Map, foldWithKey, fromList, lookup, mapKeysMonotonic, toList)
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

parse1 :: String -> Int
parse1 = read

parse2 :: String -> Int
parse2 = parse1

hundredthDigit :: Int -> Int
hundredthDigit x = (x - (x `div` 1000) * 1000) `div` 100

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serialNum (x, y) =
    let rackId = x + 10
        pl = (y * rackId + serialNum) * rackId
    in hundredthDigit pl - 5

powerLevels :: Int -> Map (Int, Int) Int
powerLevels serialNum =
    let ps = liftA2 (,) [0 .. 300] [0 .. 300]
    in fromList $ fmap (\p -> (p, powerLevel serialNum p)) ps

nByNs :: Map (Int, Int) Int -> Int -> Map (Int, Int) Int
nByNs m n =
    let ps = liftA2 (,) [0 .. (300 - n)] [0 .. (300 - n)]
    in fromList $
       fmap
           (\p@(x, y) ->
                let s =
                        sum $
                        fmap (\p -> fromJust $ lookup p m) $
                        liftA2 (,) [x .. x + n - 1] [y .. y + n - 1]
                in (p, s))
           ps

pointWithGreatestValue :: Map (Int, Int) Int -> (Int, Int)
pointWithGreatestValue =
    fst .
    foldWithKey
        (\k' v' (k, v) ->
             if v' > v
                 then (k', v')
                 else (k, v))
        ((0, 0), 0)

answer1 :: Int -> String
answer1 = show . pointWithGreatestValue . flip nByNs 3 . powerLevels

pointWithGreatestValue' ::
       (Int, (Int, Int, Int)) -> [(Int, (Int, Int, Int))] -> (Int, Int, Int)
pointWithGreatestValue' !x [] = snd x
pointWithGreatestValue' (!v, !k) ((v', k'):t) =
    if v' > v
        then pointWithGreatestValue' (traceShow (v', k') (v', k')) t
        else pointWithGreatestValue' (v, k) t

collapse :: [(Int, Map (Int, Int) Int)] -> Map (Int, Int, Int) Int
collapse = foldMap (\(i, m) -> mapKeysMonotonic (\(a, b) -> (a, b, i)) m)

sumArea :: Map (Int, Int) Int -> (Int, Int, Int) -> Int
sumArea m (x, y, n) =
    if x + n > 300 || y + n > 300
        then 0
        else sum $
             fmap (\p -> fromJust $ lookup p m) $
             liftA2 (,) [x .. x + n - 1] [y .. y + n - 1]

-- Oof, took 3hrs, but it worked.
answer2 :: Int -> String
answer2 serialNum =
    let levels = powerLevels serialNum
    in show $
       pointWithGreatestValue' (0, (0, 0, 0)) $
       fmap (\p -> (sumArea levels p, p)) $
       liftA3 (,,) [0 .. 300] [0 .. 300] [0 .. 300]
