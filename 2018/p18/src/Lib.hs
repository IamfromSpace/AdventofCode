module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , Acre(..)
    , count
    , step
    , stepN
    ) where

import Data.List (foldr)
import Data.Map (Map, foldr, insert, keys, lookup, union)
import Prelude hiding (lookup)

data Acre
    = Ground
    | Trees
    | Lumberyard
    deriving (Eq, Ord, Show)

type Forest = Map (Int, Int) Acre

parseLine :: Int -> String -> Map (Int, Int) Acre
parseLine y =
    snd .
    Data.List.foldr
        (\c (x, m) ->
             ( x + 1
             , flip (Data.Map.insert (x, y)) m $
               case c of
                   '.' -> Ground
                   '#' -> Lumberyard
                   '|' -> Trees))
        (0, mempty)

parse1 :: String -> Forest
parse1 s =
    snd $
    Data.List.foldr
        (\l (y, m) ->
             let m' = parseLine y l
             in (y + 1, m' `union` m))
        (0, mempty) $
    lines s

match :: Maybe Acre -> Acre -> Int
match (Just Ground) Ground = 1
match (Just Trees) Trees = 1
match (Just Lumberyard) Lumberyard = 1
match _ _ = 0

countSurrounding :: Forest -> (Int, Int) -> Acre -> Int
countSurrounding forest (x, y) t =
    match (lookup (x, y + 1) forest) t + match (lookup (x, y - 1) forest) t +
    match (lookup (x + 1, y + 1) forest) t +
    match (lookup (x + 1, y - 1) forest) t +
    match (lookup (x - 1, y + 1) forest) t +
    match (lookup (x - 1, y - 1) forest) t +
    match (lookup (x - 1, y) forest) t +
    match (lookup (x + 1, y) forest) t

nextValue :: Forest -> (Int, Int) -> Acre
nextValue forest p =
    case lookup p forest of
        Just Ground ->
            if countSurrounding forest p Trees >= 3
                then Trees
                else Ground
        Just Trees ->
            if countSurrounding forest p Lumberyard >= 3
                then Lumberyard
                else Trees
        Just Lumberyard ->
            if countSurrounding forest p Lumberyard >= 1 &&
               countSurrounding forest p Trees >= 1
                then Lumberyard
                else Ground

step :: Forest -> Forest
step forest =
    Data.List.foldr (\p m -> Data.Map.insert p (nextValue forest p) m) mempty $
    keys forest

stepN :: Integer -> Forest -> Forest
stepN 0 f = f
stepN i f = stepN (i - 1) (step f)

count :: Forest -> (Int, Int)
count =
    Data.Map.foldr
        (\a (trees, lumberyards) ->
             case a of
                 Trees -> (trees + 1, lumberyards)
                 Lumberyard -> (trees, lumberyards + 1)
                 _ -> (trees, lumberyards))
        (0, 0)

parse2 :: String -> Forest
parse2 = parse1

answer1 :: Forest -> String
answer1 f = show $ uncurry (*) $ count $ stepN 10 f

findCycle ::
       Integer -> Map Forest Integer -> Forest -> (Integer, Integer, Forest)
findCycle i forests forest =
    let forest' = step forest
    in case lookup forest' forests of
           Just i' -> (i', i, forest')
           Nothing -> findCycle (i + 1) (insert forest' i forests) forest'

render :: Forest -> String
render f =
    unlines $
    map
        (\y ->
             map
                 (\x ->
                      case lookup (x, y) f of
                          Just Ground -> '.'
                          Just Trees -> '|'
                          Just Lumberyard -> '#')
                 (reverse [0 .. 9]))
        (reverse [0 .. 9])

answer2 :: Forest -> String
answer2 f =
    let (from, to, start) = findCycle 0 mempty f
        x = 1000000000 - from
        c = to - from
        cs = x `div` c
        r = x - cs * c
    -- I have no clue why I need the -1, lol
    in show $ uncurry (*) $ count $ stepN (c + r - 1) start
