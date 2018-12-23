module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import Control.Monad.Loops (iterateUntil)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Map (Map, insertWith)
import Data.Maybe (fromJust)
import Data.Sequence (Seq, deleteAt, insertAt, lookup, singleton)
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

type Parsed1 = (Int, Int)

parse1 :: String -> Parsed1
parse1 s =
    let [p, _, _, _, _, _, m, _] = words s
    in (read p, read m)

largestValue :: Map a Int -> Int
largestValue = foldr max 0

step :: Int -> State (Map Int Int, Int, Int, Seq Int) (Int, Map Int Int)
step playerCount = do
    (scores, currentIndex, lastMarble, marbles) <- get
    let thisMarble = lastMarble + 1
    let next@(scores', _, _, _) =
            if thisMarble `mod` 23 == 0
                then let newIndex = (currentIndex - 7) `mod` length marbles
                         s = fromJust $ lookup newIndex marbles
                     in ( insertWith
                              (+)
                              ((thisMarble `mod` playerCount) + 1)
                              (s + thisMarble)
                              scores
                        , newIndex
                        , thisMarble
                        , deleteAt newIndex marbles)
                else let newIndex = (currentIndex + 2) `mod` length marbles
                     in ( scores
                        , newIndex
                        , thisMarble
                        , insertAt newIndex thisMarble marbles)
    put next
    return (thisMarble, scores')

parse2 :: String -> Parsed1
parse2 = parse1

findHighScoreAt :: Int -> Int -> Int
findHighScoreAt playerCount mostValuableMarble =
    largestValue $
    snd $
    flip evalState (mempty, 0, 0, singleton 0) $
    iterateUntil ((==) mostValuableMarble . fst) (step playerCount)

answer1 :: Parsed1 -> String
answer1 = show . uncurry findHighScoreAt

answer2 :: Parsed1 -> String
answer2 = show . uncurry findHighScoreAt . fmap (* 100)
