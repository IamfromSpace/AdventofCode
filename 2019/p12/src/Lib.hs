module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , uniquePairs
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util
       (Vector(..), applyNTimes, findCycle, manLen)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List (transpose)
import Data.List.Split (splitOneOf)
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

parseLine :: String -> _
parseLine s =
    let [_:_:_:x, _:_:_:y, _:_:_:z, _] = splitOneOf ",>" s
    in Vector (read x, read y, read z)

parse1 :: String -> [Vector (Integer, Integer, Integer)]
parse1 = fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

dimGravity :: Integer -> Integer -> Integer
dimGravity d0 d1 =
    if d0 == d1
        then 0
        else if d0 < d1
                 then 1
                 else -1

uniquePairs :: [a] -> [(a, a)]
uniquePairs (h:t) = fmap ((,) h) t ++ uniquePairs t
uniquePairs [] = []

gravity ::
       Vector (Integer, Integer, Integer)
    -> Vector (Integer, Integer, Integer)
    -> Vector (Integer, Integer, Integer)
gravity (Vector (x0, y0, z0)) (Vector (x1, y1, z1)) =
    let x = dimGravity x0 x1
        y = dimGravity y0 y1
        z = dimGravity z0 z1
    in Vector (x, y, z)

step ::
       [(Vector (Integer, Integer, Integer), Vector (Integer, Integer, Integer))]
    -> [(Vector (Integer, Integer, Integer), Vector (Integer, Integer, Integer))]
step [(ap, av), (bp, bv), (cp, cv), (dp, dv)] =
    let av' = av <> foldMap (gravity ap) [bp, cp, dp]
        bv' = bv <> foldMap (gravity bp) [ap, cp, dp]
        cv' = cv <> foldMap (gravity cp) [ap, bp, dp]
        dv' = dv <> foldMap (gravity dp) [ap, bp, cp]
    in [(ap <> av', av'), (bp <> bv', bv'), (cp <> cv', cv'), (dp <> dv', dv')]

energy ::
       (Vector (Integer, Integer, Integer), Vector (Integer, Integer, Integer))
    -> Integer
energy (p, v) = manLen p * manLen v

answer1 :: _ -> _
answer1 x =
    sum $ fmap energy $ applyNTimes step (fmap (\p -> (p, mempty)) x) 1000

gravity2 :: Vector Integer -> Vector Integer -> Vector Integer
gravity2 d0 d1 =
    if d0 == d1
        then Vector 0
        else if d0 < d1
                 then Vector 1
                 else Vector (-1)

step1 ::
       [(Vector Integer, Vector Integer)] -> [(Vector Integer, Vector Integer)]
step1 [(ap, av), (bp, bv), (cp, cv), (dp, dv)] =
    let av' = av <> foldMap (gravity2 ap) [bp, cp, dp]
        bv' = bv <> foldMap (gravity2 bp) [ap, cp, dp]
        cv' = cv <> foldMap (gravity2 cp) [ap, bp, dp]
        dv' = dv <> foldMap (gravity2 dp) [ap, bp, cp]
    in [(ap <> av', av'), (bp <> bv', bv'), (cp <> cv', cv'), (dp <> dv', dv')]

split :: [Vector (Integer, Integer, Integer)] -> [[Vector Integer]]
split [(Vector (ax, ay, az)), (Vector (bx, by, bz)), (Vector (cx, cy, cz)), (Vector (dx, dy, dz))] =
    [ [Vector ax, Vector bx, Vector cx, Vector dx]
    , [Vector ay, Vector by, Vector cy, Vector dy]
    , [Vector az, Vector bz, Vector cz, Vector dz]
    ]

singleDimCycle :: [Vector Integer] -> _
singleDimCycle = findCycle step1 . fmap (\p -> (p, mempty))

-- Did the final bit by hand (wasn't positive that the cycle started at 0, so the one I would write would be wrong anyway).
answer2 :: _ -> _
answer2 x =
    let [a, b, c] = split x
    in (singleDimCycle a, singleDimCycle b, singleDimCycle c)
