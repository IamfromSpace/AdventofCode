{-# LANGUAGE BangPatterns #-}

module Lib
    ( parse
    , answer
    , remove
    , removeAll
    , removeAllSmarter
    ) where

import Data.Char

parse :: a -> a
parse = id

remove' :: Bool -> String -> Char -> String -> (String, Bool)
remove' !removed !built !last (h:t) =
    if toUpper h == toUpper last && h /= last
        then case t of
                 (h':t') -> remove' True built h' t'
                 [] -> (built, True)
        else remove' removed (built ++ [last]) h t
remove' removed built last [] = (built ++ [last], removed)

remove :: String -> (String, Bool)
remove (h:t) = remove' False "" h t
remove [] = ([], False)

removeAll :: String -> String
removeAll !s =
    let (!s', !isMore) = remove s
    in if isMore
           then removeAll s'
           else s'

removeAllSmarter :: String -> String -> String
removeAllSmarter (bh:bt) (rh:rt) =
    if toUpper bh == toUpper rh && bh /= rh
        then removeAllSmarter bt rt
        else removeAllSmarter (rh : bh : bt) rt
removeAllSmarter [] (rh:rt) = removeAllSmarter [rh] rt
removeAllSmarter b [] = b

rASOmit :: Char -> String -> String -> String
rASOmit omitted (bh:bt) (rh:rt) =
    if toUpper rh == toUpper omitted
        then rASOmit omitted (bh : bt) rt
        else if toUpper bh == toUpper rh && bh /= rh
                 then rASOmit omitted bt rt
                 else rASOmit omitted (rh : bh : bt) rt
rASOmit omitted [] (rh:rt) =
    if toUpper rh == toUpper omitted
        then rASOmit omitted [] rt
        else rASOmit omitted [rh] rt
rASOmit omitted b [] = b

-- Assume a new line
answer1 :: String -> Int
answer1 = (\x -> x - 1) . length . removeAllSmarter ""

-- Assume a new line
answer :: String -> Int
answer s =
    minimum $
    fmap ((\x -> x - 1) . length . (\c -> rASOmit c "" s)) ['a' .. 'z']
