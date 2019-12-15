module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--Note!  This one has been modified a lot as a test bed for a generalized IntCode computer
--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

import AdventOfCode.IntCode
       (Computer, WaitState(..), consume, initialize, parseInsts, step)
import AdventOfCode.Util
       (applyNTimes, elmTrace, prettyPrintPointMap)
import Control.Applicative ((<*>), liftA2)
import Control.Monad (replicateM)
import Control.Monad.Loops (unfoldWhileM)
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
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

parse1 :: String -> Map Integer Integer
parse1 = parseInsts

parse2 :: String -> _
parse2 = parse1

answer1 :: _ -> _
answer1 inst =
    Set.size $
    Set.fromList $
    fmap (\[x, y, _] -> (x, y)) $
    filter (\[_, _, x] -> x == 2) $
    chunksOf 3 $
    toList $ snd $ snd $ consume $ (\x -> (x, mempty)) $ initialize inst

data Tile
    = Wall
    | Block
    | Horizontal
    | Ball
    deriving (Show, Eq, Ord)

tileToChar :: Tile -> Char
tileToChar Block = 'X'
tileToChar Horizontal = '_'
tileToChar Wall = '#'
tileToChar Ball = '0'

-- Note:  This function was original written during the solve,
-- it became prettyPrintPointMap.
render :: (Integer, Map (Integer, Integer) Tile) -> String
render (score, m) = show score ++ "\n" ++ prettyPrintPointMap ' ' tileToChar m

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
display' sm [_] = sm

display = display' (0, mempty)

-- Either Score (BallX, Inputs Count Needed)
findBounce' :: Integer -> Computer -> Either Integer (Integer, Integer)
findBounce' n startingState =
    case consume (startingState, pure 0) of
        (_, (Halt, o)) ->
            let threes = chunksOf 3 $ toList o
            in case filter (\[x, y, _] -> x == -1 && y == 0) threes of
                   [[_, _, s]] -> Left s
                   list -> error ("no score! " ++ show list)
        (s, (Prompt, o)) ->
            let threes = chunksOf 3 $ toList o
            in case filter (\[_, y, t] -> y == 20 && t == 4) threes of
                   [[x, _, _]] -> Right (x, n + 1)
                   _ -> findBounce' (n + 1) s

findBounce :: Computer -> Either Integer (Integer, Integer)
findBounce = findBounce' 0

placePaddle :: (Computer, Integer) -> (Integer, Integer) -> (Computer, Integer)
placePaddle (startingState, x) (targetX, steps) =
    let changeSteps = abs (targetX - x)
        newInputs =
            take
                (fromIntegral (steps + 1))
                (replicate
                     (fromIntegral changeSteps)
                     (if targetX > x
                          then 1
                          else -1) ++
                 repeat 0)
    in (fst $ consume (startingState, Seq.fromList newInputs), targetX)

play :: (Computer, Integer) -> Integer
play (state, x) =
    case fmap (placePaddle (state, x)) $ findBounce state of
        Left score -> score
        Right x' -> play x'

answer2 :: Map Integer Integer -> String
answer2 inst = show $ play (initialize (Map.insert 0 2 inst), 21)
