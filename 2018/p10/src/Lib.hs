module Lib
    ( parse1
    , parseLine
    , parse2
    , answer1
    , answer2
    ) where

import Data.List.Split (splitOneOf)
import Data.Set (fromList, member)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine s =
    let [_, px, py, _, vx, vy, _] = splitOneOf "<,>" s
    in ((read px, read py), (read vx, read vy))

separate :: [(a, b)] -> ([a], [b])
separate = foldr (\(a, b) (as, bs) -> (a : as, b : bs)) ([], [])

parse1 :: String -> ([(Int, Int)], [(Int, Int)])
parse1 = separate . fmap parseLine . lines

parse2 :: String -> ([(Int, Int)], [(Int, Int)])
parse2 = parse1

step :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
step = zipWith (\(vx, vy) (px, py) -> (px + vx, py + vy))

stepCount :: Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
stepCount 0 _ ps = ps
stepCount i vs ps = stepCount (i - 1) vs (step vs ps)

render :: (Int, Int, Int, Int) -> [(Int, Int)] -> String
render (minx, maxx, miny, maxy) ps =
    let xs = [minx .. maxx]
        ys = [miny .. maxy]
        psSet = fromList ps
    in unlines $
       fmap
           (\y ->
                fmap
                    (\x ->
                         if member (x, y) psSet
                             then '#'
                             else '.')
                    xs)
           ys

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

getBoundArea :: (Int, Int, Int, Int) -> Int
getBoundArea (minx, maxx, miny, maxy) = (maxx - minx) * (maxy - miny)

stepUntilArea ::
       Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> (Int, [(Int, Int)])
stepUntilArea i maxArea vs ps =
    let ps' = step vs ps
        boundArea = getBoundArea $ getBounds ps'
    in if boundArea <= maxArea
           then (i + 1, ps')
           else stepUntilArea (i + 1) maxArea vs ps'

makeAnswer :: Int -> ([(Int, Int)], [(Int, Int)]) -> String
makeAnswer count (ps, vs) =
    let finalPs = stepCount count vs ps
        bounds = getBounds finalPs
    in render bounds finalPs

answer1 :: ([(Int, Int)], [(Int, Int)]) -> String
answer1 (ps, vs) =
    let (_, ps') = stepUntilArea 0 1000 vs ps
        bounds = getBounds ps'
    in render bounds ps'

answer2 :: ([(Int, Int)], [(Int, Int)]) -> Int
answer2 (ps, vs) =
    let (i, ps') = stepUntilArea 0 1000 vs ps
    in i
