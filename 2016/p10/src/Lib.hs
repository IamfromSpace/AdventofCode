module Lib where

import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding ((++), init, lookup, map)

data Target
    = Bot Int
    | Output Int
    deriving (Show, Eq)

data Inst
    = GoesTo Int
             Int
    | Gives Int
            Target
            Target
    deriving (Show, Eq)

type State = (Map Int [Int], Map Int [Int])

parseLine :: String -> _
parseLine s =
    case Split.splitOn " " s of
        ["value", chip, "goes", "to", "bot", bot] ->
            GoesTo (read chip) (read bot)
        ["bot", bot, "gives", "low", "to", "bot", low, "and", "high", "to", "bot", high] ->
            Gives (read bot) (Bot (read low)) (Bot (read high))
        ["bot", bot, "gives", "low", "to", "bot", low, "and", "high", "to", "output", high] ->
            Gives (read bot) (Bot (read low)) (Output (read high))
        ["bot", bot, "gives", "low", "to", "output", low, "and", "high", "to", "output", high] ->
            Gives (read bot) (Output (read low)) (Output (read high))
        ["bot", bot, "gives", "low", "to", "output", low, "and", "high", "to", "bot", high] ->
            Gives (read bot) (Output (read low)) (Bot (read high))
        _ -> error ("couldn't parse " <> s)

parse1 :: String -> _
parse1 = fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

alterAdd :: Int -> Maybe [Int] -> Maybe [Int]
alterAdd i Nothing = Just [i]
alterAdd i (Just xs) = Just (i : xs)

step :: Inst -> State -> State
step i (bots, bins) =
    case i of
        GoesTo _ _ -> (bots, bins)
        Gives bot (Bot low) (Bot high) ->
            case Map.lookup bot bots of
                Just [chip1, chip2] ->
                    let (lowChip, highChip) =
                            if chip1 > chip2
                                then (chip2, chip1)
                                else (chip1, chip2)
                    in ( Map.alter (alterAdd highChip) high $
                         Map.alter (alterAdd lowChip) low $
                         Map.insert bot [] bots
                       , bins)
                _ -> (bots, bins)
        Gives bot (Bot low) (Output high) ->
            case Map.lookup bot bots of
                Just [chip1, chip2] ->
                    let (lowChip, highChip) =
                            if chip1 > chip2
                                then (chip2, chip1)
                                else (chip1, chip2)
                    in ( Map.alter (alterAdd lowChip) low $
                         Map.insert bot [] bots
                       , Map.alter (alterAdd highChip) high $ bins)
                _ -> (bots, bins)
        Gives bot (Output low) (Output high) ->
            case Map.lookup bot bots of
                Just [chip1, chip2] ->
                    let (lowChip, highChip) =
                            if chip1 > chip2
                                then (chip2, chip1)
                                else (chip1, chip2)
                    in ( Map.insert bot [] bots
                       , Map.alter (alterAdd lowChip) low $
                         Map.alter (alterAdd highChip) high $ bins)
                _ -> (bots, bins)
        Gives bot (Output low) (Bot high) ->
            case Map.lookup bot bots of
                Just [chip1, chip2] ->
                    let (lowChip, highChip) =
                            if chip1 > chip2
                                then (chip2, chip1)
                                else (chip1, chip2)
                    in ( Map.alter (alterAdd highChip) high $
                         Map.insert bot [] bots
                       , Map.alter (alterAdd lowChip) low $ bins)
                _ -> (bots, bins)

steps :: [Inst] -> State -> State
steps is state = List.foldl' (flip step) state is

init' :: Inst -> Map Int [Int] -> Map Int [Int]
init' i bots =
    case i of
        Gives _ _ _ -> bots
        GoesTo chip bot -> Map.alter (alterAdd chip) bot bots

init :: [Inst] -> Map Int [Int]
init = List.foldl' (flip init') mempty

findMicros :: Map Int [Int] -> [Int]
findMicros bots =
    List.take 1 $
    List.filter
        (\x ->
             let v = Map.lookup x bots
             in v == Just [61, 17] || v == Just [17, 61])
        (Map.keys bots)

-- 37:03
answer1 :: _ -> _
answer1 insts =
    findMicros $
    fst $
    Util.boundedUntil
        1000
        ((\x -> x > 0) . List.length . findMicros . fst)
        (steps insts)
        ((init insts), mempty)

-- 42:29
answer2 :: _ -> _
answer2 insts =
    let outs =
            snd $
            Util.boundedUntil
                10000
                ((\outs ->
                      Map.lookup 0 outs /= Nothing &&
                      Map.lookup 1 outs /= Nothing &&
                      Map.lookup 2 outs /= Nothing) .
                 snd)
                (steps insts)
                ((init insts), mempty)
        Just [zero] = Map.lookup 0 outs
        Just [one] = Map.lookup 1 outs
        Just [two] = Map.lookup 2 outs
    in zero * one * two

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: _
ex1_1 = undefined

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: _
ex2_1 = undefined

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
