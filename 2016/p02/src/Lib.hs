{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , findKey
    ) where

--import Prelude hiding (lookup)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util
       (BoundingBox(..), Vector(..), elmTrace, intoBoundingBox,
        isBoundedBy)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Data.List (foldl')
import Data.List.Split ()
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

type Parsed1 = [[Vector (Integer, Integer)]]

type Parsed2 = Parsed1

parseChar :: Char -> Vector (Integer, Integer)
parseChar 'U' = Vector (0, 1)
parseChar 'D' = Vector (0, -1)
parseChar 'L' = Vector (-1, 0)
parseChar 'R' = Vector (1, 0)
parseChar _ = error "dead"

parse1 :: String -> Parsed1
parse1 = fmap (fmap parseChar) . lines

boundedAdd ::
       BoundingBox (Integer, Integer)
    -> Vector (Integer, Integer)
    -> Vector (Integer, Integer)
    -> Vector (Integer, Integer)
boundedAdd bounds start added =
    let Vector x = start <> added
    in if isBoundedBy x bounds
           then Vector x
           else start

parse2 :: String -> Parsed2
parse2 = parse1

findKey ::
       Vector (Integer, Integer)
    -> [Vector (Integer, Integer)]
    -> Vector (Integer, Integer)
findKey =
    foldl' (boundedAdd (intoBoundingBox (-1, -1) <> intoBoundingBox (1, 1)))

vecToNum :: Vector (Integer, Integer) -> Int
vecToNum (Vector (-1, 1)) = 1
vecToNum (Vector (0, 1)) = 2
vecToNum (Vector (1, 1)) = 3
vecToNum (Vector (-1, 0)) = 4
vecToNum (Vector (0, 0)) = 5
vecToNum (Vector (1, 0)) = 6
vecToNum (Vector (-1, -1)) = 7
vecToNum (Vector (0, -1)) = 8
vecToNum (Vector (1, -1)) = 9
vecToNum _ = error "shit!"

-- Time to complete: 35:11.83
answer1 :: Parsed1 -> String
answer1 =
    show .
    fmap vecToNum .
    drop 1 .
    reverse .
    foldl'
        (\found vecs ->
             let nextKey = findKey (head found) vecs
             in nextKey : found)
        [(Vector (0, 0))]

isAllowed :: Vector (Integer, Integer) -> Bool
isAllowed (Vector (0, 2)) = True
isAllowed (Vector (-1, 1)) = True
isAllowed (Vector (0, 1)) = True
isAllowed (Vector (1, 1)) = True
isAllowed (Vector (-2, 0)) = True
isAllowed (Vector (-1, 0)) = True
isAllowed (Vector (0, 0)) = True
isAllowed (Vector (1, 0)) = True
isAllowed (Vector (2, 0)) = True
isAllowed (Vector (-1, -1)) = True
isAllowed (Vector (0, -1)) = True
isAllowed (Vector (1, -1)) = True
isAllowed (Vector (0, -2)) = True
isAllowed _ = False

boundedAdd2 ::
       Vector (Integer, Integer)
    -> Vector (Integer, Integer)
    -> Vector (Integer, Integer)
boundedAdd2 start added =
    let x = start <> added
    in if isAllowed x
           then x
           else start

findKey2 ::
       Vector (Integer, Integer)
    -> [Vector (Integer, Integer)]
    -> Vector (Integer, Integer)
findKey2 = foldl' boundedAdd2

vecToNum2 :: Vector (Integer, Integer) -> Char
vecToNum2 (Vector (0, 2)) = '1'
vecToNum2 (Vector (-1, 1)) = '2'
vecToNum2 (Vector (0, 1)) = '3'
vecToNum2 (Vector (1, 1)) = '4'
vecToNum2 (Vector (-2, 0)) = '5'
vecToNum2 (Vector (-1, 0)) = '6'
vecToNum2 (Vector (0, 0)) = '7'
vecToNum2 (Vector (1, 0)) = '8'
vecToNum2 (Vector (2, 0)) = '9'
vecToNum2 (Vector (-1, -1)) = 'A'
vecToNum2 (Vector (0, -1)) = 'B'
vecToNum2 (Vector (1, -1)) = 'C'
vecToNum2 (Vector (0, -2)) = 'D'
vecToNum2 _ = error "shit!"

-- Time to complete: 43:06.4
answer2 :: Parsed2 -> String
answer2 =
    show .
    fmap vecToNum2 .
    drop 1 .
    reverse .
    foldl'
        (\found vecs ->
             let nextKey = findKey2 (head found) vecs
             in nextKey : found)
        [(Vector (-2, 0))]
