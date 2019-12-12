module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

import AdventOfCode.Util (elmTrace)
import Control.Applicative ((<*>), liftA2)
import Control.Monad (replicateM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Monad.State.Lazy (State, evalState, get, runState)
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List (permutations)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as Map (insert, lookup)
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
                    ( relB
                    , tail inputs
                    , Just (i + 2)
                    , writeR ma relB inst a (head inputs)
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

data Dir
    = N
    | S
    | E
    | W
    deriving (Ord, Eq, Show)

move :: (Integer, Integer) -> Dir -> Integer -> ((Integer, Integer), Dir)
move (x, y) N 0 = ((x - 1, y), W)
move (x, y) N 1 = ((x + 1, y), E)
move (x, y) S 0 = ((x + 1, y), E)
move (x, y) S 1 = ((x - 1, y), W)
move (x, y) E 0 = ((x, y + 1), N)
move (x, y) E 1 = ((x, y - 1), S)
move (x, y) W 0 = ((x, y - 1), S)
move (x, y) W 1 = ((x, y + 1), N)
move _ _ _ = error "Bad turn!"

loop ::
       [((Integer, Integer), Dir)]
    -> (Integer, Integer)
    -> Dir
    -> Set (Integer, Integer)
    -> (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
    -> _
loop path position facing whitePanels state
   -- I think this is the right order for turn/color (backwards-ish)
 =
    case until (\(_, _, z, _, x) -> length x == 2 || z == Nothing) step state of
        (_, _, Nothing, _, _) -> path
        (a, b, c@(Just _), d, [turn, color]) ->
            let (position', facing') = move position facing turn
                whitePanels' =
                    case color of
                        0 -> Set.delete position whitePanels
                        1 -> Set.insert position whitePanels
                        _ -> error "Bad color!"
                nextPanelColorCode =
                    if Set.member position' whitePanels'
                        then 1
                        else 0
                path' = (position', facing') : path
            in loop
                   path'
                   position'
                   facing'
                   whitePanels'
                   (a, [nextPanelColorCode], c, d, [])

answer1 :: _ -> _
answer1 inst =
    Set.size $
    Set.fromList $
    fmap fst $ loop [((0, 0), N)] (0, 0) N mempty (0, [0], Just 0, inst, [])

loop2 ::
       (Integer, Integer)
    -> Dir
    -> Set (Integer, Integer)
    -> (Integer, [Integer], Maybe Integer, Map Integer Integer, [Integer])
    -> _
loop2 position facing whitePanels state
   -- I think this is the right order for turn/color (backwards-ish)
 =
    case until (\(_, _, z, _, x) -> length x == 2 || z == Nothing) step state of
        (_, _, Nothing, _, _) -> whitePanels
        (a, b, c@(Just _), d, [turn, color]) ->
            let (position', facing') = move position facing turn
                whitePanels' =
                    case color of
                        0 -> Set.delete position whitePanels
                        1 -> Set.insert position whitePanels
                        _ -> error "Bad color!"
                nextPanelColorCode =
                    if Set.member position' whitePanels'
                        then 1
                        else 0
            in loop2
                   position'
                   facing'
                   whitePanels'
                   (a, [nextPanelColorCode], c, d, [])

prettyPrint :: Set (Integer, Integer) -> _
prettyPrint s =
    let xs = fmap fst $ Set.toList s
        ys = fmap snd $ Set.toList s
        (minX, minY) = (minimum xs, minimum ys)
        (maxX, maxY) = (maximum xs, maximum ys)
    in unlines $
       chunksOf (fromIntegral maxX - fromIntegral minX + 1) $
       fmap
           (\(y, x) ->
                if Set.member (x, y) s
                    then 'X'
                    else ' ') $
       liftA2 (,) (reverse [minY .. maxY]) [minX .. maxX]

answer2 :: _ -> _
answer2 inst =
    prettyPrint $
    loop2 (0, 0) N (Set.singleton (0, 0)) (0, [1], Just 0, inst, [])
