{-# LANGUAGE BangPatterns #-}

module Lib
    ( parse1
    , answer1
    , answer2
    ) where

import Data.Maybe (catMaybes)
import qualified Data.Vector.Mutable as V
import Debug.Trace (traceShow)

type Parsed1 = Int

parse1 :: String -> Parsed1
--parse1 = fmap (\c -> read [c]) . filter (/= '\n')
parse1 = read

-- lazy, lol
toDigits :: Int -> [Int]
toDigits = fmap (\c -> read [c]) . show

addRec :: ([Int], [Int]) -> ([Int], [Int])
addRec (!elfIndexes, !recs) =
    let s = sum $ fmap (\i -> recs !! i) elfIndexes
        recs' = recs ++ toDigits s
        nextLength = length recs'
        elfIndexes' =
            fmap
                (\i ->
                     mod (recs !! i + i + 1) $
                     if mod nextLength 1000 == 0
                         then traceShow nextLength nextLength
                         else nextLength)
                elfIndexes
    in (elfIndexes', recs')

stepUntil :: Int -> ([Int], [Int]) -> ([Int], [Int])
stepUntil len state@(_, rs) =
    if length rs >= len
        then state
        else stepUntil len (addRec state)

addRec' ::
       ((Int, Int), V.IOVector Int, Int) -> IO ((Int, Int), V.IOVector Int, Int)
addRec' ((i1, i2), recs, len) = do
    v1 <- V.read recs i1
    v2 <- V.read recs i2
    let s = v1 + v2
    newLen <-
        case toDigits s of
            [a, b] -> do
                V.write recs len a
                V.write recs (len + 1) b
                return $ 2 + len
            [a] -> do
                V.write recs len a
                return $ 1 + len
    return ((mod (v1 + i1 + 1) newLen, mod (v2 + i2 + 1) newLen), recs, newLen)

stepUntil' :: Int -> ((Int, Int), V.IOVector Int, Int) -> IO ()
stepUntil' maxLen state@(_, _, len) =
    if len >= maxLen
        then return ()
        else do
            state' <- addRec' state
            stepUntil' maxLen state'
            return ()

answer1' :: Parsed1 -> String
answer1' x =
    let (_, recs) = stepUntil (x + 10) ([0, 1], [3, 7])
    in fmap (head . show) $ take 10 $ drop x recs

answer1 :: Parsed1 -> IO String
answer1 x = do
    recs <- V.new (x + 12)
    V.write recs 0 3
    V.write recs 1 7
    stepUntil' (x + 10) ((0, 1), recs, 2)
    let recs' = V.drop x recs
    v <- traverse (V.read recs') [0 .. 9]
    return $ fmap (head . show) v

matches :: V.IOVector Int -> Int -> IO (Maybe Int)
matches v i = do
    b <- traverse (V.read v) [i .. i + 5]
    return $
        if b == [8, 9, 0, 6, 9, 1]
            then Just i
            else Nothing

contains :: Int -> V.IOVector Int -> IO (Maybe Int)
contains len v = do
    ms <- traverse (matches v) [max (len - 8) 0 .. len - 6]
    return $
        case catMaybes ms of
            [] -> Nothing
            (a:_) -> Just a

stepUntil'' :: ((Int, Int), V.IOVector Int, Int) -> IO Int
stepUntil'' state@(_, v, len) = do
    done <- contains len v
    case done of
        Just len -> return len
        Nothing -> do
            state' <- addRec' state
            stepUntil'' state'

-- Haskell is not a great language for this problem :/
answer2 :: IO String
answer2 = do
    recs <- V.new 100000000
    V.write recs 0 3
    V.write recs 1 7
    i <- stepUntil'' ((0, 1), recs, 2)
    return (show i)
