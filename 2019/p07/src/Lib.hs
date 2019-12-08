module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , prog
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util (elmTrace)
import Control.Applicative ((<*>))
import Control.Monad (replicateM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Monad.State.Lazy
       (State, evalState, get, put, runState)
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List (permutations)
import Data.List.Split (splitOn)
import qualified Data.Map as Map ()
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (fromList, lookup, update)
import qualified Data.Set as Set ()

parse1 :: String -> Seq Int
parse1 = Seq.fromList . fmap read . splitOn ","

parse2 :: String -> _
parse2 = parse1

step :: State ([Int], Maybe Int, Seq Int) (Maybe Int, Bool)
step = do
    state <- get
    case state of
        (inputs, Nothing, inst) -> return (Nothing, True)
        (inputs, Just i, inst) ->
            case ( show <$> Seq.lookup i inst
                 , Seq.lookup (i + 1) inst
                 , Seq.lookup (i + 2) inst
                 , Seq.lookup (i + 3) inst) of
                (Just "1", Just a, Just b, Just c) -> do
                    put
                        ( inputs
                        , Just (i + 4)
                        , Seq.update
                              c
                              (fromJust (Seq.lookup a inst) +
                               fromJust (Seq.lookup b inst))
                              inst)
                    return (Nothing, False)
                (Just "101", Just a, Just b, Just c) -> do
                    put
                        ( inputs
                        , Just (i + 4)
                        , Seq.update c (a + fromJust (Seq.lookup b inst)) inst)
                    return (Nothing, False)
                (Just "1001", Just a, Just b, Just c) -> do
                    put
                        ( inputs
                        , Just (i + 4)
                        , Seq.update c (fromJust (Seq.lookup a inst) + b) inst)
                    return (Nothing, False)
                (Just "1101", Just a, Just b, Just c) -> do
                    put (inputs, Just (i + 4), Seq.update c (a + b) inst)
                    return (Nothing, False)
                (Just "2", Just a, Just b, Just c) -> do
                    put
                        ( inputs
                        , Just (i + 4)
                        , Seq.update
                              c
                              (fromJust (Seq.lookup a inst) *
                               fromJust (Seq.lookup b inst))
                              inst)
                    return (Nothing, False)
                (Just "102", Just a, Just b, Just c) -> do
                    put
                        ( inputs
                        , Just (i + 4)
                        , Seq.update c (a * fromJust (Seq.lookup b inst)) inst)
                    return (Nothing, False)
                (Just "1002", Just a, Just b, Just c) -> do
                    put
                        ( inputs
                        , Just (i + 4)
                        , Seq.update c (fromJust (Seq.lookup a inst) * b) inst)
                    return (Nothing, False)
                (Just "1102", Just a, Just b, Just c) -> do
                    put (inputs, Just (i + 4), Seq.update c (a * b) inst)
                    return (Nothing, False)
                (Just "3", Just a, _, _) -> do
                    put
                        ( tail inputs
                        , Just (i + 2)
                        , Seq.update a (head inputs) inst)
                    return (Nothing, False)
                (Just "4", Just a, _, _) -> do
                    put (inputs, Just (i + 2), inst)
                    return ((Just (fromJust $ Seq.lookup a inst)), False)
                (Just "104", Just a, _, _) -> do
                    put (inputs, Just (i + 2), inst)
                    return ((Just a), False)
                (Just "5", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) /= 0
                        then do
                            put
                                ( inputs
                                , Just (fromJust $ Seq.lookup b inst)
                                , inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just "105", Just a, Just b, _) ->
                    if a /= 0
                        then do
                            put
                                ( inputs
                                , Just (fromJust $ Seq.lookup b inst)
                                , inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just "1005", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) /= 0
                        then do
                            put (inputs, Just b, inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just "1105", Just a, Just b, _) ->
                    if a /= 0
                        then do
                            put (inputs, Just b, inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just "6", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) == 0
                        then do
                            put
                                ( inputs
                                , Just (fromJust $ Seq.lookup b inst)
                                , inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just "106", Just a, Just b, _) ->
                    if a == 0
                        then do
                            put
                                ( inputs
                                , Just (fromJust $ Seq.lookup b inst)
                                , inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just "1006", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) == 0
                        then do
                            put (inputs, Just b, inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just "1106", Just a, Just b, _) ->
                    if a == 0
                        then do
                            put (inputs, Just b, inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just "7", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) <
                       (fromJust $ Seq.lookup b inst)
                        then do
                            put (inputs, Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "107", Just a, Just b, Just c) ->
                    if a < (fromJust $ Seq.lookup b inst)
                        then do
                            put (inputs, Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "1007", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) < b
                        then do
                            put (inputs, Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "1107", Just a, Just b, Just c) ->
                    if a < b
                        then do
                            put (inputs, Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "8", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) ==
                       (fromJust $ Seq.lookup b inst)
                        then do
                            put (inputs, Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "108", Just a, Just b, Just c) ->
                    if a == (fromJust $ Seq.lookup b inst)
                        then do
                            put (inputs, Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "1008", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) == b
                        then do
                            put (inputs, Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "1108", Just a, Just b, Just c) ->
                    if a == b
                        then do
                            put (inputs, Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (inputs, Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "99", _, _, _) -> do
                    put (inputs, Nothing, inst)
                    return (Nothing, True)
                (Just x, _, _, _) -> error $ show (x, i, inst)
                (_, _, _, _) -> error $ show (i, inst)

computer :: [Int] -> Seq Int -> _
computer inputs inst
    --flip runState (sid, Just 0, inst) $ replicateM count $ step
 =
    last $
    catMaybes $
    fmap fst $
    flip evalState (inputs, Just 0, inst) $ unfoldWhileM (not . snd) step

amps :: Seq Int -> (Int, Int, Int, Int, Int) -> _
amps insts (a, b, c, d, e) =
    elmTrace $
    computer
        [ e
        , computer
              [d, computer [c, computer [b, computer [a, 0] insts] insts] insts]
              insts
        ]
        insts

prog :: Int -> [Int] -> Seq Int -> _
prog count inputs inst =
    let (a, b) = flip runState (inputs, Just 0, inst) $ replicateM count $ step
    in (b, a)

answer1 :: _ -> _
answer1 inst
  --amps inst (4, 4, 4, 4, 4)
 =
    maximum $
    fmap (\x -> (amps inst x, x)) $
    fmap (\[a, b, c, d, e] -> (a, b, c, d, e)) $ permutations [0 .. 4]

step2 :: State (Bool, [Int], Maybe Int, Seq Int) (Maybe Int, Bool, Bool)
step2 = do
    state <- get
    case state of
        (halted, inputs, Nothing, inst) -> return (Nothing, True, False)
        (_, inputs, Just i, inst) ->
            case ( show <$> Seq.lookup i inst
                 , Seq.lookup (i + 1) inst
                 , Seq.lookup (i + 2) inst
                 , Seq.lookup (i + 3) inst) of
                (Just "1", Just a, Just b, Just c) -> do
                    put
                        ( False
                        , inputs
                        , Just (i + 4)
                        , Seq.update
                              c
                              (fromJust (Seq.lookup a inst) +
                               fromJust (Seq.lookup b inst))
                              inst)
                    return (Nothing, False, False)
                (Just "101", Just a, Just b, Just c) -> do
                    put
                        ( False
                        , inputs
                        , Just (i + 4)
                        , Seq.update c (a + fromJust (Seq.lookup b inst)) inst)
                    return (Nothing, False, False)
                (Just "1001", Just a, Just b, Just c) -> do
                    put
                        ( False
                        , inputs
                        , Just (i + 4)
                        , Seq.update c (fromJust (Seq.lookup a inst) + b) inst)
                    return (Nothing, False, False)
                (Just "1101", Just a, Just b, Just c) -> do
                    put (False, inputs, Just (i + 4), Seq.update c (a + b) inst)
                    return (Nothing, False, False)
                (Just "2", Just a, Just b, Just c) -> do
                    put
                        ( False
                        , inputs
                        , Just (i + 4)
                        , Seq.update
                              c
                              (fromJust (Seq.lookup a inst) *
                               fromJust (Seq.lookup b inst))
                              inst)
                    return (Nothing, False, False)
                (Just "102", Just a, Just b, Just c) -> do
                    put
                        ( False
                        , inputs
                        , Just (i + 4)
                        , Seq.update c (a * fromJust (Seq.lookup b inst)) inst)
                    return (Nothing, False, False)
                (Just "1002", Just a, Just b, Just c) -> do
                    put
                        ( False
                        , inputs
                        , Just (i + 4)
                        , Seq.update c (fromJust (Seq.lookup a inst) * b) inst)
                    return (Nothing, False, False)
                (Just "1102", Just a, Just b, Just c) -> do
                    put (False, inputs, Just (i + 4), Seq.update c (a * b) inst)
                    return (Nothing, False, False)
                (Just "3", Just a, _, _) -> do
                    case inputs of
                        [] -> return (Nothing, False, True)
                        (h:t) -> do
                            put (False, t, Just (i + 2), Seq.update a h inst)
                            return (Nothing, False, False)
                (Just "4", Just a, _, _) -> do
                    put (False, inputs, Just (i + 2), inst)
                    return ((Just (fromJust $ Seq.lookup a inst)), False, False)
                (Just "104", Just a, _, _) -> do
                    put (False, inputs, Just (i + 2), inst)
                    return ((Just a), False, False)
                (Just "5", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) /= 0
                        then do
                            put
                                ( False
                                , inputs
                                , Just (fromJust $ Seq.lookup b inst)
                                , inst)
                            return (Nothing, False, False)
                        else do
                            put (False, inputs, Just (i + 3), inst)
                            return (Nothing, False, False)
                (Just "105", Just a, Just b, _) ->
                    if a /= 0
                        then do
                            put
                                ( False
                                , inputs
                                , Just (fromJust $ Seq.lookup b inst)
                                , inst)
                            return (Nothing, False, False)
                        else do
                            put (False, inputs, Just (i + 3), inst)
                            return (Nothing, False, False)
                (Just "1005", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) /= 0
                        then do
                            put (False, inputs, Just b, inst)
                            return (Nothing, False, False)
                        else do
                            put (False, inputs, Just (i + 3), inst)
                            return (Nothing, False, False)
                (Just "1105", Just a, Just b, _) ->
                    if a /= 0
                        then do
                            put (False, inputs, Just b, inst)
                            return (Nothing, False, False)
                        else do
                            put (False, inputs, Just (i + 3), inst)
                            return (Nothing, False, False)
                (Just "6", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) == 0
                        then do
                            put
                                ( False
                                , inputs
                                , Just (fromJust $ Seq.lookup b inst)
                                , inst)
                            return (Nothing, False, False)
                        else do
                            put (False, inputs, Just (i + 3), inst)
                            return (Nothing, False, False)
                (Just "106", Just a, Just b, _) ->
                    if a == 0
                        then do
                            put
                                ( False
                                , inputs
                                , Just (fromJust $ Seq.lookup b inst)
                                , inst)
                            return (Nothing, False, False)
                        else do
                            put (False, inputs, Just (i + 3), inst)
                            return (Nothing, False, False)
                (Just "1006", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) == 0
                        then do
                            put (False, inputs, Just b, inst)
                            return (Nothing, False, False)
                        else do
                            put (False, inputs, Just (i + 3), inst)
                            return (Nothing, False, False)
                (Just "1106", Just a, Just b, _) ->
                    if a == 0
                        then do
                            put (False, inputs, Just b, inst)
                            return (Nothing, False, False)
                        else do
                            put (False, inputs, Just (i + 3), inst)
                            return (Nothing, False, False)
                (Just "7", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) <
                       (fromJust $ Seq.lookup b inst)
                        then do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 1 inst)
                            return (Nothing, False, False)
                        else do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 0 inst)
                            return (Nothing, False, False)
                (Just "107", Just a, Just b, Just c) ->
                    if a < (fromJust $ Seq.lookup b inst)
                        then do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 1 inst)
                            return (Nothing, False, False)
                        else do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 0 inst)
                            return (Nothing, False, False)
                (Just "1007", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) < b
                        then do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 1 inst)
                            return (Nothing, False, False)
                        else do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 0 inst)
                            return (Nothing, False, False)
                (Just "1107", Just a, Just b, Just c) ->
                    if a < b
                        then do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 1 inst)
                            return (Nothing, False, False)
                        else do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 0 inst)
                            return (Nothing, False, False)
                (Just "8", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) ==
                       (fromJust $ Seq.lookup b inst)
                        then do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 1 inst)
                            return (Nothing, False, False)
                        else do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 0 inst)
                            return (Nothing, False, False)
                (Just "108", Just a, Just b, Just c) ->
                    if a == (fromJust $ Seq.lookup b inst)
                        then do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 1 inst)
                            return (Nothing, False, False)
                        else do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 0 inst)
                            return (Nothing, False, False)
                (Just "1008", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) == b
                        then do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 1 inst)
                            return (Nothing, False, False)
                        else do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 0 inst)
                            return (Nothing, False, False)
                (Just "1108", Just a, Just b, Just c) ->
                    if a == b
                        then do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 1 inst)
                            return (Nothing, False, False)
                        else do
                            put
                                ( False
                                , inputs
                                , Just (i + 4)
                                , Seq.update c 0 inst)
                            return (Nothing, False, False)
                (Just "99", _, _, _) -> do
                    put (True, inputs, Nothing, inst)
                    return (Nothing, True, False)
                (Just x, _, _, _) -> error $ show (x, i, inst)
                (_, _, _, _) -> error $ show (i, inst)

computer2 :: _ -> _
computer2 inState =
    let (ma, state) =
            flip runState inState $
            unfoldWhileM (\(a, b, c) -> not b && not c) step2
        (didHalt, _, _, _) = state
        outputs = catMaybes $ fmap (\(a, _, _) -> a) ma
    in (state, outputs, didHalt)

ins :: [Int]
    -> (Bool, [Int], Maybe Int, Seq Int)
    -> (Bool, [Int], Maybe Int, Seq Int)
ins new (b, old, c, d) = (b, old ++ new, c, d)

amps2 :: _ -> _
amps2 (as, bs, cs, ds, es) =
    let (nexta, ra, ha) = computer2 as
        (nextb, rb, hb) = computer2 (ins ra bs)
        (nextc, rc, hc) = computer2 (ins rb cs)
        (nextd, rd, hd) = computer2 (ins rc ds)
        (nexte, re, he) = computer2 (ins rd es)
    in if ha && hb && hc && hd && he
           then last re
           else amps2 (ins re nexta, nextb, nextc, nextd, nexte)

answer2 :: _ -> _
answer2 inst =
    maximum $
    fmap
        (\[a, b, c, d, e] ->
             ( amps2 $
               ( (False, [a, 0], Just 0, inst)
               , (False, [b], Just 0, inst)
               , (False, [c], Just 0, inst)
               , (False, [d], Just 0, inst)
               , (False, [e], Just 0, inst))
             , (a, b, c, d, e))) $
    permutations [5 .. 9]
