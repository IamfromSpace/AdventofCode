module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util ()
import Control.Applicative ()
import Control.Monad.Loops (unfoldWhileM)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
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

step :: Int -> State (Maybe Int, Seq Int) (Maybe Int, Bool)
step input = do
    state <- get
    case state of
        (Nothing, inst) -> return (Nothing, True)
        (Just i, inst) ->
            case ( show <$> Seq.lookup i inst
                 , Seq.lookup (i + 1) inst
                 , Seq.lookup (i + 2) inst
                 , Seq.lookup (i + 3) inst) of
                (Just "1", Just a, Just b, Just c) -> do
                    put
                        ( Just (i + 4)
                        , Seq.update
                              c
                              (fromJust (Seq.lookup a inst) +
                               fromJust (Seq.lookup b inst))
                              inst)
                    return (Nothing, False)
                (Just "101", Just a, Just b, Just c) -> do
                    put
                        ( Just (i + 4)
                        , Seq.update c (a + fromJust (Seq.lookup b inst)) inst)
                    return (Nothing, False)
                (Just "1001", Just a, Just b, Just c) -> do
                    put
                        ( Just (i + 4)
                        , Seq.update c (fromJust (Seq.lookup a inst) + b) inst)
                    return (Nothing, False)
                (Just "1101", Just a, Just b, Just c) -> do
                    put (Just (i + 4), Seq.update c (a + b) inst)
                    return (Nothing, False)
                (Just "2", Just a, Just b, Just c) -> do
                    put
                        ( Just (i + 4)
                        , Seq.update
                              c
                              (fromJust (Seq.lookup a inst) *
                               fromJust (Seq.lookup b inst))
                              inst)
                    return (Nothing, False)
                (Just "102", Just a, Just b, Just c) -> do
                    put
                        ( Just (i + 4)
                        , Seq.update c (a * fromJust (Seq.lookup b inst)) inst)
                    return (Nothing, False)
                (Just "1002", Just a, Just b, Just c) -> do
                    put
                        ( Just (i + 4)
                        , Seq.update c (fromJust (Seq.lookup a inst) * b) inst)
                    return (Nothing, False)
                (Just "1102", Just a, Just b, Just c) -> do
                    put (Just (i + 4), Seq.update c (a * b) inst)
                    return (Nothing, False)
                (Just "3", Just a, _, _) -> do
                    put (Just (i + 2), Seq.update a input inst)
                    return (Nothing, False)
                (Just "4", Just a, _, _) -> do
                    put (Just (i + 2), inst)
                    return ((Just (fromJust $ Seq.lookup a inst)), False)
                (Just "104", Just a, _, _) -> do
                    put (Just (i + 2), inst)
                    return ((Just a), False)
                (Just "5", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) /= 0
                        then do
                            put (Just (fromJust $ Seq.lookup b inst), inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 3), inst)
                            return (Nothing, False)
                (Just "105", Just a, Just b, _) ->
                    if a /= 0
                        then do
                            put (Just (fromJust $ Seq.lookup b inst), inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 3), inst)
                            return (Nothing, False)
                (Just "1005", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) /= 0
                        then do
                            put (Just b, inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 3), inst)
                            return (Nothing, False)
                (Just "1105", Just a, Just b, _) ->
                    if a /= 0
                        then do
                            put (Just b, inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 3), inst)
                            return (Nothing, False)
                (Just "6", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) == 0
                        then do
                            put (Just (fromJust $ Seq.lookup b inst), inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 3), inst)
                            return (Nothing, False)
                (Just "106", Just a, Just b, _) ->
                    if a == 0
                        then do
                            put (Just (fromJust $ Seq.lookup b inst), inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 3), inst)
                            return (Nothing, False)
                (Just "1006", Just a, Just b, _) ->
                    if (fromJust $ Seq.lookup a inst) == 0
                        then do
                            put (Just b, inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 3), inst)
                            return (Nothing, False)
                (Just "1106", Just a, Just b, _) ->
                    if a == 0
                        then do
                            put (Just b, inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 3), inst)
                            return (Nothing, False)
                (Just "7", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) <
                       (fromJust $ Seq.lookup b inst)
                        then do
                            put (Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "107", Just a, Just b, Just c) ->
                    if a < (fromJust $ Seq.lookup b inst)
                        then do
                            put (Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "1007", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) < b
                        then do
                            put (Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "1107", Just a, Just b, Just c) ->
                    if a < b
                        then do
                            put (Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "8", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) ==
                       (fromJust $ Seq.lookup b inst)
                        then do
                            put (Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "108", Just a, Just b, Just c) ->
                    if a == (fromJust $ Seq.lookup b inst)
                        then do
                            put (Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "1008", Just a, Just b, Just c) ->
                    if (fromJust $ Seq.lookup a inst) == b
                        then do
                            put (Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "1108", Just a, Just b, Just c) ->
                    if a == b
                        then do
                            put (Just (i + 4), Seq.update c 1 inst)
                            return (Nothing, False)
                        else do
                            put (Just (i + 4), Seq.update c 0 inst)
                            return (Nothing, False)
                (Just "99", _, _, _) -> do
                    put (Nothing, inst)
                    return (Nothing, True)
                (Just x, _, _, _) -> error $ show (x, i, inst)
                (_, _, _, _) -> error $ show (i, inst)

computer :: Int -> Seq Int -> _
computer sid inst =
    last $
    catMaybes $
    fmap fst $
    flip evalState (Just 0, inst) $ unfoldWhileM (not . snd) $ step sid

answer1 :: _ -> _
answer1 = computer 1

answer2 :: _ -> _
answer2 = computer 5
