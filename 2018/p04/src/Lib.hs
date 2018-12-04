module Lib
    ( parse
    , answer
    ) where

import Data.List (foldl')
import Data.List.Split
import Data.Map hiding (drop, foldl')

data Event
    = NewGuard Int
    | Sleep
    | Wake
    deriving (Show)

parseLine :: String -> (Integer, (Event, Int))
parseLine s =
    let [t, e] = splitOn "]" $ drop 1 s
        [ys, mos, ds, hs, ms] = splitOneOf "- :" t
        event =
            case e of
                " falls asleep" -> Sleep
                " wakes up" -> Wake
                _ ->
                    let [_, ids, _, _] = words e
                    in NewGuard $ read $ drop 1 ids
    in ( (((read ys * 12 + read mos) * 40 + read ds) * 24 + read hs) * 60 +
         read ms
       , (event, read ms))

parse :: String -> Map Integer (Event, Int)
parse s = fromList $ fmap parseLine (lines s)

f :: (Int, Integer, Map Int Integer)
  -> Integer
  -> (Event, Int)
  -> (Int, Integer, Map Int Integer)
f (guard, lastT, acc) time (event, minut) =
    case event of
        NewGuard newGuard -> (newGuard, time, acc)
        Sleep -> (guard, time, acc)
        Wake -> (guard, time, insertWith (+) guard (time - lastT) acc)

agg :: Map Integer (Event, Int) -> Map Int Integer
agg m =
    let (_, _, m') = foldlWithKey f (0, 0, mempty) m
    in m'

findMaxValue :: (Ord a, Ord b) => (a, b) -> Map a b -> (a, b)
findMaxValue =
    foldlWithKey
        (\(k, v) k' v' ->
             if v' > v
                 then (k', v')
                 else (k, v))

aggByMin :: Int -> Map Integer (Event, Int) -> Map Int Int
aggByMin guardId =
    (\(x, _, _) -> x) .
    foldl'
        (\(m, correctGuard, lastMin) (e, min) ->
             case e of
                 NewGuard newGuard ->
                     if newGuard == guardId
                         then (m, True, min)
                         else (m, False, min)
                 Sleep -> (m, correctGuard, min)
                 Wake ->
                     if correctGuard
                         then ( foldl'
                                    (\acc k -> insertWith (+) k 1 acc)
                                    m
                                    [lastMin .. min]
                              , correctGuard
                              , min)
                         else (m, correctGuard, min))
        (mempty, False, 0)

sleepiestGuard :: Map Integer (Event, Int) -> Int
sleepiestGuard = fst . findMaxValue (-1, -1) . agg

sleepiestMinuteForGuard :: Int -> Map Integer (Event, Int) -> Int
sleepiestMinuteForGuard guardId = fst . findMaxValue (-1, -1) . aggByMin guardId

answer1 :: Map Integer (Event, Int) -> Int
answer1 m =
    let guardId = sleepiestGuard m
    in guardId * sleepiestMinuteForGuard guardId m

aggByGuardAndMin :: Map Integer (Event, Int) -> Map (Int, Int) Int
aggByGuardAndMin =
    (\(x, _, _) -> x) .
    foldl'
        (\(m, currentGuard, lastMin) (e, min) ->
             case e of
                 NewGuard newGuard -> (m, newGuard, min)
                 Sleep -> (m, currentGuard, min)
                 Wake ->
                     ( foldl'
                           (\acc k -> insertWith (+) (currentGuard, k) 1 acc)
                           m
                           [lastMin .. min]
                     , currentGuard
                     , min))
        (mempty, -1, 0)

answer :: Map Integer (Event, Int) -> Int
answer m =
    let (guardId, min) = fst $ findMaxValue ((-1, -1), -1) $ aggByGuardAndMin m
    in guardId * min
