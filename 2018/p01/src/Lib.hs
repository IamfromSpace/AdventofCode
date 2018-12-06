module Lib
    ( answer
    , parse
    ) where

import Data.List (cycle)
import Data.Set

p :: String -> Int
p ('+':t) = read t :: Int
p ('-':t) = -1 * read t :: Int
p _ = undefined

parse :: String -> [Int]
parse s = fmap p (lines s)

answer1 :: [Int] -> Int
answer1 = sum

answer' :: Set Int -> Int -> [Int] -> Int
answer' seen running (h:t) =
    let running' = running + h
    in if member running' seen
           then running'
           else answer' (insert running' seen) running' t
answer' _ _ [] = error "list should be infinite!"

answer :: [Int] -> Int
answer = answer' mempty 0 . cycle
