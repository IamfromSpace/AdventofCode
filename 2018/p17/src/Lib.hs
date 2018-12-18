{-# LANGUAGE BangPatterns #-}

module Lib
    ( parse1
    , parse2
    , parseLine
    , answer1'
    , answer1
    , answer2
    , step
    ) where

import Data.List.Split (splitOneOf)
import Data.Set
       (Set, fromList, insert, member, notMember, size, toList, union)
import Debug.Trace (traceShow)

type Parsed1 = Set (Int, Int)

type Parsed2 = String

parseLine :: String -> [(Int, Int)]
parseLine s =
    case splitOneOf ",." s of
        ['x':'=':x, ' ':'y':'=':y0, _, y1] ->
            fmap ((,) (read x)) [read y0 .. read y1]
        ['y':'=':y, ' ':'x':'=':x0, _, x1] ->
            fmap (flip (,) (read y)) [read x0 .. read x1]

parse1 :: String -> Parsed1
parse1 x = fromList (lines x >>= parseLine)

parse2 :: String -> Parsed1
parse2 = parse1

lowestY :: Set (Int, Int) -> Int
lowestY = maximum . fmap snd . toList

highestY :: Set (Int, Int) -> Int
highestY = minimum . fmap snd . toList

-- Returns the fill boundary, and if it drips
findBoundary ::
       (Int -> Int)
    -> Bool
    -> Set (Int, Int)
    -> Set (Int, Int)
    -> (Int, Int)
    -> (Int, Bool)
findBoundary op canOverflow clay water (x, y) =
    if member (op x, y) clay
        then (x, False)
        else if notMember (op x, y + 1) clay && canOverflow
                 then if member (op $ op x, y) water
                   --filled before
                          then findBoundary op False clay water (op x, y)
                          else (op x, True)
                 else findBoundary
                          op
                          (canOverflow || member (op x, y + 1) clay)
                          clay
                          water
                          (op x, y)

findBoundaryLeft ::
       Set (Int, Int) -> Set (Int, Int) -> (Int, Int) -> (Int, Bool)
findBoundaryLeft clay water (x, y) =
    findBoundary (\x -> x - 1) (member (x, y + 1) clay) clay water (x, y)

findBoundaryRight ::
       Set (Int, Int) -> Set (Int, Int) -> (Int, Int) -> (Int, Bool)
findBoundaryRight clay water (x, y) =
    findBoundary (\x -> x + 1) (member (x, y + 1) clay) clay water (x, y)

stepDrip ::
       Set (Int, Int)
    -> (Set (Int, Int), (Int, Int))
    -> (Set (Int, Int), [(Int, Int)], [(Int, Int)])
stepDrip clay (water, (x, y)) =
    if (y + 1) > lowestY clay
        then (water, [], [])
        else if member (x, y + 1) clay
                 then (water, [], [(x, y)])
                 else (insert (x, y + 1) water, [(x, y + 1)], [])

stepFill ::
       Set (Int, Int)
    -> (Set (Int, Int), (Int, Int))
    -> (Set (Int, Int), [(Int, Int)], [(Int, Int)])
stepFill clay (water, (x, y)) =
    let (left, leftOverflows) = findBoundaryLeft clay water (x, y)
        (right, rightOverflows) = findBoundaryRight clay water (x, y)
        leftDrip =
            if leftOverflows
                then [(left, y)]
                else []
        rightDrip =
            if rightOverflows
                then [(right, y)]
                else []
        drips = leftDrip ++ rightDrip
        fills =
            if not leftOverflows && not rightOverflows
                then [(x, y - 1)]
                else []
    in (foldr (\xx -> insert (xx, y)) water [left .. right], drips, fills)

step ::
       Set (Int, Int)
    -> (Set (Int, Int), Set (Int, Int), [(Int, Int)], [(Int, Int)])
    -> Set (Int, Int)
step _ (water, _, [], []) = water
step clay (!water, !dripHistory, !drips, fill:fills) =
    let (water', newDrips, newFills) =
            traceShow (size water) $ stepFill clay (water, fill)
        allowedDrips = filter (\d -> notMember d dripHistory) newDrips
        dripHistory' = dripHistory `union` fromList allowedDrips
    in step
           clay
           (water', dripHistory', drips ++ allowedDrips, fills ++ newFills)
step clay (!water, !dripHistory, drip:drips, !fills) =
    let (water', newDrips, newFills) =
            traceShow (size water) $ stepDrip clay (water, drip)
        allowedDrips = filter (\d -> notMember d dripHistory) newDrips
        dripHistory' = dripHistory `union` fromList allowedDrips
    in step
           clay
           (water', dripHistory', drips ++ allowedDrips, fills ++ newFills)

answer1' :: [(Int, Int)] -> Parsed1 -> String
answer1' spouts clay =
    let water = step clay (fromList spouts, fromList spouts, spouts, [])
        smallest = highestY clay
    in show $ length $ filter (\(_, y) -> y >= smallest) $ toList water

render :: Set (Int, Int) -> Set (Int, Int) -> String
render clay water =
    let (minx, maxx, miny, maxy) = getBounds $ toList water
    in unlines $
       fmap
           (\y ->
                (fmap
                     (\x ->
                          if member (x, y) clay
                              then '#'
                              else if member (x, y) water
                                       then '~'
                                       else '.'))
                    [minx - 4 .. maxx + 4])
           [miny - 4 .. maxy + 4]

answer1 :: Parsed1 -> String
answer1 = answer1' [(500, 0)]
  {-
 =
    let spouts = [(500, 0)]
    in render clay $ step clay (fromList spouts, fromList spouts, spouts, [])
    -}

touchesClay ::
       (Int -> Int) -> Set (Int, Int) -> Set (Int, Int) -> (Int, Int) -> Bool
touchesClay op clay water (x, y) =
    if member (op x, y) clay
        then True
        else if member (op x, y) water
                 then touchesClay op clay water (op x, y)
                 else False

answer2' :: [(Int, Int)] -> Parsed1 -> String
answer2' spouts clay =
    let water = step clay (fromList spouts, fromList spouts, spouts, [])
        smallest = highestY clay
        wList = filter (\(_, y) -> y >= smallest) $ toList water
    in show $
       length $
       filter
           (\p ->
                touchesClay (\x -> x - 1) clay water p &&
                touchesClay (\x -> x + 1) clay water p)
           wList

answer2 :: Parsed1 -> String
answer2 = answer2' [(500, 0)]

---
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
