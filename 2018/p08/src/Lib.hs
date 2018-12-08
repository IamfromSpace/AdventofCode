module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , sumMetaData
    , Tree(..)
    , partTwoMetaData
    , readTree
    ) where

import Data.Map (Map, insert, lookup)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

data Tree =
    Tree [Tree]
         [Int]
    deriving (Show)

trace' :: (Show a) => String -> a -> a
trace' label x = trace (label ++ ": " ++ show x) x

parse1 :: String -> [Int]
parse1 = fmap read . words

parse2 :: String -> [Int]
parse2 = parse1

--Something something state monad...
readTrees :: Int -> [Tree] -> [Int] -> ([Tree], [Int])
readTrees i ts is =
    if i == 0
        then (ts, is)
        else let (tree, is') = readTree is
             in readTrees (i - 1) (ts ++ [tree]) is'

readTree :: [Int] -> (Tree, [Int])
readTree (nodeCount:metaDataCount:t) =
    let (nodes, t') = readTrees nodeCount [] t
        (metaData, t'') = splitAt metaDataCount t'
    in (Tree nodes metaData, t'')

sumMetaData :: Tree -> Int
sumMetaData (Tree ts md) = sum (map sumMetaData ts) + sum md

answer1 :: [Int] -> Int
answer1 = sumMetaData . fst . readTree

toMap :: [a] -> Map Int a
toMap = fst . foldl (\(m, i) v -> (insert i v m, i + 1)) (mempty, 0)

partTwoMetaData :: Tree -> Int
partTwoMetaData (Tree [] metaData) = sum metaData
partTwoMetaData (Tree nodes metaData) =
    let nodeIndexMap = toMap $ fmap partTwoMetaData nodes
    in sum $
       fmap (\i -> fromMaybe 0 $ Data.Map.lookup (i - 1) nodeIndexMap) metaData

answer2 :: [Int] -> Int
answer2 = partTwoMetaData . fst . readTree
