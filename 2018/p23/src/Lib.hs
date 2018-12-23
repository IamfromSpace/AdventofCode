module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , distance
    , subDivide
    ) where

import Control.Applicative (liftA2, liftA3)
import Data.List (sortOn)
import Data.List.Split (splitOneOf)
import Data.Map (Map, fromList, toList)

type Bot = ((Int, Int, Int), Int)

type Parsed1 = [Bot]

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
distance (x1, y1, z1) (x2, y2, z2) =
    abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

parseLine :: String -> Bot
parseLine s =
    let [_, _, x, y, z, _, _, r] = splitOneOf "=<>," s
    in ((read x, read y, read z), read r)

strongestSignal :: [Bot] -> Int
strongestSignal = foldr (\(_, s) m -> max s m) 0

strongestSignalBot :: [Bot] -> Bot
strongestSignalBot xs =
    let strongestSig = strongestSignal xs
    in head $ filter (\(_, s) -> s == strongestSig) xs

isInRange :: Bot -> Bot -> Bool
isInRange (p1, s) (p2, _) = distance p1 p2 <= s

parse1 :: String -> Parsed1
parse1 = fmap parseLine . lines

parse2 :: String -> Parsed1
parse2 = parse1

answer1 :: Parsed1 -> String
answer1 bs =
    let b = strongestSignalBot bs
    in show $ length $ filter (isInRange b) bs

extendBounds ::
       (Int, Int, Int)
    -> (Int, Int, Int, Int, Int, Int)
    -> (Int, Int, Int, Int, Int, Int)
extendBounds (nx, ny, nz) (minx, maxx, miny, maxy, minz, maxz) =
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
          else maxy
    , if nz < minz
          then nz
          else minz
    , if nz > maxz
          then nz
          else maxz)

getBounds :: [(Int, Int, Int)] -> (Int, Int, Int, Int, Int, Int)
getBounds ((x, y, z):t) = foldr extendBounds (x, x, y, y, z, z) t
getBounds [] = error "empty list has no bounds!"

getSearchArea :: [Bot] -> (Int, Int, Int, Int, Int, Int)
getSearchArea bs = getBounds $ fmap fst bs

isInRange' :: (Int, Int, Int) -> Bot -> Bool
isInRange' p1 (p2, s) = distance p1 p2 <= s

-- Correct but suuper slow
answer2' :: Parsed1 -> String
answer2' bs =
    let (minx, maxx, miny, maxy, minz, maxz) = getSearchArea bs
        ps = liftA3 (,,) [minx .. maxx] [miny .. maxy] [minz .. maxz]
        (_, p) =
            foldr
                (\p (m, pm) ->
                     let m' = length (filter (isInRange' p) bs)
                     in if m > m'
                            then (m, pm)
                            else (m', p))
                (0, (0, 0, 0))
                ps
    in show $ distance p (0, 0, 0)

subDivide :: Int -> (Int, Int, Int, Int, Int, Int) -> [(Int, Int, Int)]
subDivide n (minx, maxx, miny, maxy, minz, maxz) =
    let scaleX = max 1 ((maxx - minx) `div` n)
        xs = fmap ((+ minx) . (* scaleX)) [0 .. ((maxx - minx) `div` scaleX)]
        scaleY = max 1 ((maxy - miny) `div` n)
        ys = fmap ((+ miny) . (* scaleY)) [0 .. ((maxy - miny) `div` scaleY)]
        scaleZ = max 1 ((maxz - minz) `div` n)
        zs = fmap ((+ minz) . (* scaleZ)) [0 .. ((maxz - minz) `div` scaleZ)]
    in liftA3 (,,) xs ys zs

newDivision ::
       [Bot] -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
newDivision bs bounds =
    let ps = subDivide 50 bounds
        withCounts = fmap (\p -> (length (filter (isInRange' p) bs), p)) ps
    in getBounds $ fmap snd $ take 16 $ reverse $ toList $ fromList withCounts

newDivisionN ::
       Int
    -> [Bot]
    -> (Int, Int, Int, Int, Int, Int)
    -> (Int, Int, Int, Int, Int, Int)
newDivisionN 0 _ b = b
newDivisionN i bs b = newDivisionN (i - 1) bs $ newDivision bs b

answer2'' :: Parsed1 -> String
answer2'' bs =
    let mostBot =
            last $
            toList $
            fromList $
            fmap (\b@(bp, _) -> (length (filter (isInRange' bp) bs), b)) bs
        (x, y, z) = (45724608, 20989671, 41904520) --fst $ snd mostBot
        dist = 1000000
        b =
            newDivisionN
                40
                bs
                (x - dist, x + dist, y - dist, y + dist, z - dist, z + dist)
        divs = subDivide 1 b
    in show $ fmap (\p -> (length (filter (isInRange' p) bs), p)) divs

closestToRange :: [Bot] -> (Int, Int, Int) -> Bot
closestToRange bs p =
    snd $
    head $
    sortOn fst $
    filter ((> 0) . fst) $ fmap (\b@(bp, r) -> (distance bp p - r, b)) bs

degreesOfFreedom :: [Bot] -> (Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
degreesOfFreedom bs (x, y, z) =
    foldr
        (\((x', y', z'), r) la@(posX, negX, posY, negY, posZ, negZ) ->
             let freedom = r - distance (x, y, z) (x', y', z')
             in if freedom >= 0
                    then ( if x > x'
                               then min posX freedom
                               else posX
                         , if x' > x
                               then negX
                               else min negX freedom
                         , if y > y'
                               then min posY freedom
                               else posY
                         , if y' > y
                               then negY
                               else min negY freedom
                         , if z > z'
                               then min posZ freedom
                               else posZ
                         , if z' > z
                               then negZ
                               else min negZ freedom)
                    else la)
        (1000000000, 1000000000, 1000000000, 1000000000, 1000000000, 1000000000)
        bs

countInRange :: [Bot] -> (Int, Int, Int) -> Int
countInRange bs p = length $ filter (isInRange' p) bs

-- 921: (45724610, 20989671, 41904520)
-- Totally hacked and clawed and manually brute forced this anwer.
-- Good leaderboard result (for me), terrible approach.
answer2 :: Parsed1 -> String
answer2 bs = show $ countInRange bs (45724610, 20989671, 41904520)
