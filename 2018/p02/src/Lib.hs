module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Control.Applicative ((<|>), liftA2)
import Data.List (zipWith)
import Data.Map (Map, insertWith, singleton)
import Data.Monoid (Any, Any(Any), Sum(Sum), getAny, getSum)

type Parsed1 = [String]

parse1 :: String -> Parsed1
parse1 = lines

parse2 :: String -> Parsed1
parse2 = parse1

stringToCountMap :: String -> Map Char (Sum Int)
stringToCountMap = foldr (\c -> insertWith (<>) c (Sum 1)) mempty

toNCount :: Int -> Map a (Sum Int) -> Any
toNCount n = foldMap (\v -> Any (v == Sum n))

sumAnys :: [Any] -> Int
sumAnys =
    getSum .
    foldMap
        (\a ->
             Sum
                 (if getAny a
                      then 1
                      else 0))

answer1 :: Parsed1 -> String
answer1 s =
    let m = fmap stringToCountMap s
        withTwos = sumAnys $ fmap (toNCount 2) m
        withThrees = sumAnys $ fmap (toNCount 3) m
    in show $ withTwos * withThrees

countDifferentChars :: String -> String -> Int
countDifferentChars as =
    sum .
    zipWith
        (\a b ->
             if a == b
                 then 0
                 else 1)
        as

answer2 :: Parsed1 -> String
answer2 strs =
    show $
    foldr
        (\(as, bs) x ->
             x <|>
             if countDifferentChars as bs == 1
                 then Just (as, bs)
                 else Nothing)
        Nothing $
    liftA2 (,) strs strs
