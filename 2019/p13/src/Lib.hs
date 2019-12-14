module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , run
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

import AdventOfCode.Util (applyNTimes, elmTrace)
import Control.Applicative ((<*>), liftA2)
import Control.Monad (replicateM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Monad.State.Lazy (State, evalState, get, runState)
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List (cycle, permutations)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as Map
       (delete, fromList, insert, lookup, toList)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Monoid ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (fromList, lookup, update)
import Data.Set (Set)
import qualified Data.Set as Set
       (delete, fromList, insert, member, singleton, size, toList)

listToIndexMap :: [a] -> Map Integer a
listToIndexMap =
    fst . foldl (\(m, i) v -> (Map.insert i v m, i + 1)) (mempty, 0)

parse1 :: String -> Map Integer Integer
parse1 = listToIndexMap . fmap read . splitOn ","

parse2 :: String -> _
parse2 = parse1

data Mode
    = Immediate
    | Position
    | Relative
    deriving (Show)

decodeOp :: Integer -> (Mode, Mode, Mode, Integer)
decodeOp x =
    let toMode z =
            case z of
                0 -> Position
                1 -> Immediate
                2 -> Relative
                _ -> error ("Bad mode! " ++ show z)
    in ( toMode $ (x `div` 100) `mod` 10
       , toMode $ (x `div` 1000) `mod` 10
       , toMode $ (x `div` 10000) `mod` 10
       , x `mod` 100)

readR :: Mode -> Integer -> Map Integer Integer -> Integer -> Integer
readR mode relativeBase insts value =
    case mode of
        Immediate -> value
        Position -> fromMaybe 0 $ Map.lookup value insts
        Relative -> fromMaybe 0 $ Map.lookup (relativeBase + value) insts

writeR ::
       Mode
    -> Integer
    -> Map Integer Integer
    -> Integer
    -> Integer
    -> Map Integer Integer
writeR mode relativeBase insts index value =
    case mode of
        Immediate -> error "Immediate write!"
        Position -> Map.insert index value insts
        Relative -> Map.insert (relativeBase + index) value insts

step ::
       (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
    -> (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
step state =
    case state of
        (_, _, Nothing, _, _) -> state
        (relB, inputs, Just i, inst, outputs) ->
            case ( decodeOp <$> Map.lookup i inst
                 , fromMaybe 0 $ Map.lookup (i + 1) inst
                 , fromMaybe 0 $ Map.lookup (i + 2) inst
                 , fromMaybe 0 $ Map.lookup (i + 3) inst) of
                (Just (ma, mb, mc, 1), a, b, c) ->
                    ( relB
                    , inputs
                    , Just (i + 4)
                    , writeR
                          mc
                          relB
                          inst
                          c
                          (readR ma relB inst a + readR mb relB inst b)
                    , outputs)
                (Just (ma, mb, mc, 2), a, b, c) ->
                    ( relB
                    , inputs
                    , Just (i + 4)
                    , writeR
                          mc
                          relB
                          inst
                          c
                          (readR ma relB inst a * readR mb relB inst b)
                    , outputs)
                (Just (ma, _, _, 3), a, _, _) ->
                    case inputs of
                        [] -> state
                        (h:t) ->
                            ( relB
                            , t
                            , Just (i + 2)
                            , writeR ma relB inst a h
                            , outputs)
                (Just (ma, _, _, 4), a, _, _) ->
                    ( relB
                    , inputs
                    , Just (i + 2)
                    , inst
                    , (readR ma relB inst a) : outputs)
                (Just (ma, mb, _, 5), a, b, _) ->
                    if readR ma relB inst a /= 0
                        then ( relB
                             , inputs
                             , Just (readR mb relB inst b)
                             , inst
                             , outputs)
                        else (relB, inputs, Just (i + 3), inst, outputs)
                (Just (ma, mb, _, 6), a, b, _) ->
                    if readR ma relB inst a == 0
                        then ( relB
                             , inputs
                             , Just (readR mb relB inst b)
                             , inst
                             , outputs)
                        else (relB, inputs, Just (i + 3), inst, outputs)
                (Just (ma, mb, mc, 7), a, b, c) ->
                    if readR ma relB inst a < readR mb relB inst b
                        then ( relB
                             , inputs
                             , Just (i + 4)
                             , writeR mc relB inst c 1
                             , outputs)
                        else ( relB
                             , inputs
                             , Just (i + 4)
                             , writeR mc relB inst c 0
                             , outputs)
                (Just (ma, mb, mc, 8), a, b, c) ->
                    if readR ma relB inst a == readR mb relB inst b
                        then ( relB
                             , inputs
                             , Just (i + 4)
                             , writeR mc relB inst c 1
                             , outputs)
                        else ( relB
                             , inputs
                             , Just (i + 4)
                             , writeR mc relB inst c 0
                             , outputs)
                (Just (ma, _, _, 9), a, _, _) ->
                    ( relB + readR ma relB inst a
                    , inputs
                    , Just (i + 2)
                    , inst
                    , outputs)
                (Just (_, _, _, 99), _, _, _) ->
                    (relB, inputs, Nothing, inst, outputs)
                (Just x, _, _, _) -> error $ show (x, i, inst)
                (_, _, _, _) -> error $ show (i, inst)

answer1 :: _ -> _
answer1 inst =
    Set.size $
    Set.fromList $
    fmap (\[x, y, _] -> (x, y)) $
    filter (\[_, _, x] -> x == 2) $
    chunksOf 3 $
    reverse $
    (\(_, _, _, _, o) -> o) $
    until (\(_, _, a, _, _) -> a == Nothing) step (0, [], Just 0, inst, [])

data Tile
    = Wall
    | Block
    | Horizontal
    | Ball
    deriving (Show, Eq, Ord)

render :: (Integer, Map (Integer, Integer) Tile) -> String
render (score, m) =
    let xs = fmap (fst . fst) $ Map.toList m
        ys = fmap (snd . fst) $ Map.toList m
        (minX, minY) = (minimum xs, minimum ys)
        (maxX, maxY) = (maximum xs, maximum ys)
    in (++) (show score ++ "\n") $
       unlines $
       chunksOf (fromIntegral maxX - fromIntegral minX + 1) $
       fmap
           (\(y, x) ->
                case Map.lookup (x, y) m of
                    Nothing -> ' '
                    Just Block -> 'X'
                    Just Horizontal -> '_'
                    Just Wall -> '#'
                    Just Ball -> '0') $
       liftA2 (,) (reverse [minY .. maxY]) [minX .. maxX]

display' ::
       (Integer, Map (Integer, Integer) Tile)
    -> [[Integer]]
    -> (Integer, Map (Integer, Integer) Tile)
display' sm [] = sm
display' (score, m) ([-1, 0, c]:t) = display' (c, m) t
display' (score, m) ([x, y, 0]:t) = display' (score, Map.delete (x, y) m) t
display' (score, m) ([x, y, 1]:t) = display' (score, Map.insert (x, y) Wall m) t
display' (score, m) ([x, y, 2]:t) =
    display' (score, Map.insert (x, y) Block m) t
display' (score, m) ([x, y, 3]:t) =
    display' (score, Map.insert (x, y) Horizontal m) t
display' (score, m) ([x, y, 4]:t) = display' (score, Map.insert (x, y) Ball m) t
display' sm (_:[]) = sm

display = display' (0, mempty)

findBounce ::
       (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
    -> (Integer, Integer)
findBounce startingState =
    (\(_, inp, _, _, (t:y:x:_)) -> (x, fromIntegral (5000 - length inp))) $
    until
        (\(_, _, x, _, c) ->
             x == Nothing ||
             case c of
                 (t:y:x:pt:py:_) -> pt == 0 && t == 4 && y == 20 && py == 19
                 _ -> False)
        step $
    (\(a, b, c, d, e) -> (a, take 5000 (repeat 0), c, d, e)) startingState

placePaddle ::
       ( (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
       , Integer)
    -> ( (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
       , Integer)
placePaddle (startingState, x) =
    let (targetX, steps) = findBounce startingState
        changeSteps = abs (targetX - x)
        newInputs =
            take
                (fromIntegral (steps + 1))
                ((take (fromIntegral changeSteps) $
                  repeat
                      (if targetX > x
                           then 1
                           else -1)) ++
                 repeat 0)
    in ( until
             (\(_, inp, x, _, c) ->
                  x == Nothing ||
                  case c of
                      (t:y:x:pt:py:_) ->
                          py == 20 &&
                          pt == 0 && t == 4 && y == 19 && length inp == 0
                      _ -> False)
             step $
         (\(a, b, c, d, e) -> (a, b ++ newInputs, c, d, e)) startingState
       , targetX)

run :: [Integer] -> Map Integer Integer -> String
run input inst =
    render $
    display $
    chunksOf 3 $
    reverse $
    (\(_, _, _, _, o) -> o) $
    applyNTimes step (0, input, Just 0, Map.insert 0 2 inst, []) 512000

start ::
       Map Integer Integer
    -> (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
start inst =
    until
        (\(_, x, _, _, _) -> length x == 0)
        step
        (0, [0], Just 0, Map.insert 0 2 inst, [])

play ::
       Integer
    -> (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
    -> (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
play paddlePos state =
    fst $
    until (\((_, _, x, _, _), _) -> x == Nothing) placePaddle (state, paddlePos)

answer2 :: Map Integer Integer -> String
answer2 inst =
    render $
    display $
    chunksOf 3 $
    reverse $
    (\(_, _, _, _, o) -> o) $ -- play 21 $ start inst
    until (\(a, b, c, d, e) -> c == Nothing) step $
    fst $ applyNTimes placePaddle (start inst, 21) 218
