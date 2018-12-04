module Lib
    ( parse
    , p
    , noOverlap
    , answer
    , findDelim
    , allPositions
    , noOverlap
    ) where

import Control.Applicative

findDelim' :: String -> Char -> String -> (String, String)
findDelim' left c (h:t) =
    if h == c
        then (left, t)
        else findDelim' (left ++ [h]) c t
findDelim' left _ [] = (left, "")

findDelim = findDelim' ""

p :: String -> (Int, Int, Int, Int)
p ('#':t) =
    let [id, "@", p, d] = words t
        (x, yc) = findDelim ',' p
        (y, _) = findDelim ':' yc
        (w, h) = findDelim 'x' d
    in (read x, read y, read w, read h)

parse :: String -> [(Int, Int, Int, Int)]
parse s = fmap p (lines s)

inClaim :: (Int, Int) -> (Int, Int, Int, Int) -> Bool
inClaim (x, y) (cx, cy, cw, ch) =
    x >= cx && x < cx + cw && y >= cy && y < cy + ch

allPositions = liftA2 (,) [0 .. 1000] [0 .. 1000]

twoOrMore' :: Bool -> (a -> Bool) -> [a] -> Bool
twoOrMore' False fn (h:t) = twoOrMore' (fn h) fn t
twoOrMore' True fn (h:t) = fn h || twoOrMore' True fn t
twoOrMore' _ _ [] = False

twoOrMore = twoOrMore' False

answer1 :: [(Int, Int, Int, Int)] -> Int
answer1 claims =
    foldl
        (\c b ->
             c +
             if b
                 then 1
                 else 0)
        0 $
    fmap (\p -> twoOrMore (inClaim p) claims) allPositions

answer' ::
       Int
    -> Int
    -> [(Int, Int, Int, Int)]
    -> [(Int, Int, Int, Int)]
    -> [(Int, Int, Int, Int)]
    -> Int
answer' currId nextId all (current:toTry) (next:toExplore) =
    if noOverlap current next || currId == nextId
        then answer' currId (nextId + 1) all (current : toTry) toExplore
        else answer' (currId + 1) 0 all toTry all
answer' currId _ _ _ [] = currId + 1
answer' _ _ _ _ _ = error "no matches"

answer :: [(Int, Int, Int, Int)] -> Int
answer x = answer' 0 0 x x x

noOverlap :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
noOverlap (x1, y1, w1, h1) (x2, y2, w2, h2) =
    (x1 + w1 <= x2 || x2 + w2 <= x1) || (y1 + h1 <= y2 || y2 + h2 <= y1)
