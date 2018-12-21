{-# LANGUAGE BangPatterns #-}

module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Data.Map (Map, fromList, union)
import Data.Semigroup (Min(Min), Sum(Sum), getMin, getSum)

type Parsed1 = String

type Parsed2 = String

parse1 :: String -> Parsed1
parse1 = id

parse2 :: String -> Parsed2
parse2 = id

parseSet :: [Int] -> Int -> Bool -> String -> (Int, String)
parseSet !totals !this _ ('N':t) = parseSet totals (this + 1) False t
parseSet !totals !this _ ('S':t) = parseSet totals (this + 1) False t
parseSet !totals !this _ ('E':t) = parseSet totals (this + 1) False t
parseSet !totals !this _ ('W':t) = parseSet totals (this + 1) False t
parseSet !totals !this _ ('|':t) = parseSet (this : totals) 0 True t
parseSet !totals !this sawPipe (')':t) =
    if sawPipe
        then (0, t)
        else (maximum (this : totals), t)
parseSet !totals !this _ ('$':t) = (maximum (this : totals), t)
parseSet !totals !this _ ('(':t) =
    let (v, t') = parseSet [] 0 False t
    in parseSet totals (this + v) False t'

answer1 :: Parsed1 -> String
answer1 = show . fst . parseSet [] 0 False . drop 1

type Vector = (Sum Int, Sum Int)

addHistory :: Int -> Map Vector (Min Int) -> Map Vector (Min Int)
addHistory prevRooms = fmap (\x -> Min (prevRooms + getMin x))

mkMap ::
       [Vector]
    -> [Vector]
    -> Map Vector (Min Int)
    -> [Vector]
    -> [Int]
    -> Int
    -> Bool
    -> String
    -> (Map Vector (Min Int), [Vector], Int, String)
mkMap fPs iPs rooms ps !totals !this _ ('|':t) =
    mkMap (ps ++ fPs) iPs rooms iPs (this : totals) 0 True t
mkMap fPs iPs rooms ps !totals !this sawPipe (')':t) =
    if sawPipe
        then (rooms, iPs, 0, t)
        else (rooms, fPs ++ ps, maximum (this : totals), t)
mkMap fPs iPs rooms ps !totals !this _ ('$':t) =
    (rooms, ps ++ fPs, maximum (this : totals), t)
mkMap fPs iPs rooms ps !totals !this _ ('(':t) =
    let (rooms', ps', v, t') = mkMap [] ps mempty ps [] 0 False t
    in mkMap
           fPs
           iPs
           (rooms `union` addHistory this rooms')
           ps'
           totals
           (this + v)
           False
           t'
mkMap fPs iPs rooms ps !totals !this _ (x:t) =
    let this' = this + 1
        v =
            case x of
                'N' -> (Sum 0, Sum 1)
                'S' -> (Sum 0, Sum (-1))
                'E' -> (Sum 1, Sum 0)
                'W' -> (Sum (-1), Sum 0)
        ps' = fmap (<> v) ps
        rooms' = rooms `union` fromList (fmap (\p -> (p, Min this')) ps')
    in mkMap fPs iPs rooms' ps' totals this' False t

mkMap' :: String -> Map Vector (Min Int)
mkMap' =
    (\(x, _, _, _) -> x) .
    mkMap [] [(Sum 0, Sum 0)] mempty [(Sum 0, Sum 0)] [] 0 False

countValuesOver1000 :: Map a (Min Int) -> Int
countValuesOver1000 =
    getSum .
    foldMap
        (\x ->
             if getMin x >= 1000
                 then Sum 1
                 else Sum 0)

answer2 :: Parsed2 -> String
answer2 = show . countValuesOver1000 . mkMap' . drop 1
