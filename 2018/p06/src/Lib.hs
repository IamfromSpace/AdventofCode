module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Control.Applicative
import Data.List.Split
import Data.Map hiding (delete, filter, foldr, fromList, member)
import Data.Set hiding (filter, foldr)

parse1 :: String -> [(Int, Int)]
parse1 =
    fmap
        (\s ->
             let [x, y] = splitOn "," s
             in (read x, read y)) .
    lines

parse2 = parse1

extendBounds :: (Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
extendBounds (nx, ny) (minx, maxx, miny, maxy) =
    ( if nx < minx
          then nx
          else minx
    , if nx > maxx
          then nx
          else maxx
    , if ny < miny
          then ny
          else miny
    , if ny > maxy
          then ny
          else maxy)

getBounds :: [(Int, Int)] -> (Int, Int, Int, Int)
getBounds ((x, y):t) = foldr extendBounds (x, x, y, y) t
getBounds [] = error "empty list has no bounds!"

genAllPoints :: (Int, Int, Int, Int) -> [(Int, Int)]
genAllPoints (minx, maxx, miny, maxy) = liftA2 (,) [minx .. maxx] [miny .. maxy]

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

shortest' ::
       Bool -> Int -> Int -> Int -> [(Int, Int)] -> (Int, Int) -> Maybe Int
shortest' seenTwo shortestDist i thisI (h:t) p =
    let thisDist = dist h p
    in if thisDist < shortestDist
           then shortest' False thisDist thisI (thisI + 1) t p
           else if thisDist == shortestDist
                    then shortest' True thisDist thisI (thisI + 1) t p
                    else shortest' seenTwo shortestDist i (thisI + 1) t p
shortest' seenTwo _ i _ [] _ =
    if seenTwo
        then Nothing
        else Just i

shortest :: [(Int, Int)] -> (Int, Int) -> Maybe Int
shortest = shortest' False 1000000 (-1) 0

isUnbounded :: [(Int, Int)] -> (Int, Int) -> Bool
isUnbounded ps p@(x, y) =
    let others = delete p $ fromList ps
    in foldr
           (\o b -> b && (dist p (x + 100000, y) < dist o (x + 100000, y)))
           True
           others ||
       foldr
           (\o b -> b && (dist p (x - 100000, y) < dist o (x - 100000, y)))
           True
           others ||
       foldr
           (\o b -> b && (dist p (x, y + 100000) < dist o (x, y + 100000)))
           True
           others ||
       foldr
           (\o b -> b && (dist p (x, y - 100000) < dist o (x, y - 100000)))
           True
           others

agg :: [(Int, Int)] -> [(Int, Int)] -> Map Int Int
agg points =
    foldr
        (\p m ->
             case shortest points p of
                 Nothing -> m
                 Just i -> insertWith (+) i 1 m)
        mempty

findMaxValue :: (Ord a, Ord b) => (a, b) -> Map a b -> (a, b)
findMaxValue =
    foldlWithKey
        (\(k, v) k' v' ->
             if v' > v
                 then (k', v')
                 else (k, v))

boundedIndexes' :: [Int] -> Int -> [(Int, Int)] -> [(Int, Int)] -> [Int]
boundedIndexes' b i ps (h:t) =
    if isUnbounded ps h
        then boundedIndexes' b (i + 1) ps t
        else boundedIndexes' (i : b) (i + 1) ps t
boundedIndexes' b _ _ [] = b

boundedIndexes :: [(Int, Int)] -> [Int]
boundedIndexes ps = boundedIndexes' [] 0 ps ps

answer1 :: [(Int, Int)] -> Int
answer1 ps =
    let bounds = getBounds ps
        bi = fromList $ boundedIndexes ps
    in snd . findMaxValue (-1, -1) $
       filterWithKey (\k _ -> member k bi) $ agg ps $ genAllPoints bounds

totalDistance :: [(Int, Int)] -> (Int, Int) -> Int
totalDistance ps p = sum $ fmap (dist p) ps

answer2 :: [(Int, Int)] -> Int
answer2 ps =
    length $
    filter (\p -> totalDistance ps p < 10000) $
    liftA2 (,) [-1000 .. 1000] [-1000 .. 1000]
