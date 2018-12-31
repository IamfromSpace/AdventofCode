module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int, Int, Int)

type Constelation = Set Point

type Sky = Set Constelation

dist :: Point -> Point -> Int
dist (x0, y0, z0, t0) (x1, y1, z1, t1) =
    abs (x1 - x0) + abs (y1 - y0) + abs (z1 - z0) + abs (t1 - t0)

isMember :: Constelation -> Point -> Bool
isMember c p = any (\cp -> dist cp p <= 3) $ Set.toList c

addPoint :: Point -> Sky -> Sky
addPoint p s =
    let (memb, notMemb) = Set.partition (flip isMember p) s
    in Set.insert (foldMap id memb <> Set.singleton p) notMemb

type Parsed1 = [Point]

parse1 :: String -> Parsed1
parse1 s =
    (\l ->
         let [a, b, c, d] = splitOn "," l
         in (read a, read b, read c, read d)) <$>
    lines s

answer1 :: Parsed1 -> Int
answer1 = length . Set.toList . foldr addPoint mempty

type Parsed2 = String

parse2 :: String -> Parsed2
parse2 = id

answer2 :: Parsed2 -> String
answer2 _ = ""
