module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , decodeOp
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

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
import qualified Data.Map as Map (insert, lookup)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Monoid ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (fromList, lookup, update)
import qualified Data.Set as Set ()

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
       State (Integer, [Integer], Maybe Integer, Map Integer Integer) ( Maybe Integer
                                                                      , Bool)
step = do
    state <- get
    case state of
        (relB, inputs, Nothing, inst) -> return (Nothing, True)
        (relB, inputs, Just i, inst) ->
            case ( decodeOp <$> Map.lookup i inst
                 , fromMaybe 0 $ Map.lookup (i + 1) inst
                 , fromMaybe 0 $ Map.lookup (i + 2) inst
                 , fromMaybe 0 $ Map.lookup (i + 3) inst) of
                (Just (ma, mb, mc, 1), a, b, c) -> do
                    put
                        ( relB
                        , inputs
                        , Just (i + 4)
                        , writeR
                              mc
                              relB
                              inst
                              c
                              (readR ma relB inst a + readR mb relB inst b))
                    return (Nothing, False)
                (Just (ma, mb, mc, 2), a, b, c) -> do
                    put
                        ( relB
                        , inputs
                        , Just (i + 4)
                        , writeR
                              mc
                              relB
                              inst
                              c
                              (readR ma relB inst a * readR mb relB inst b))
                    return (Nothing, False)
                (Just (ma, _, _, 3), a, _, _) -> do
                    put
                        ( relB
                        , tail inputs
                        , Just (i + 2)
                        , writeR ma relB inst a (head inputs))
                    return (Nothing, False)
                (Just (ma, _, _, 4), a, _, _) -> do
                    put (relB, inputs, Just (i + 2), inst)
                    return ((Just (readR ma relB inst a)), False)
                (Just (ma, mb, _, 5), a, b, _) ->
                    if readR ma relB inst a /= 0
                        then do
                            put
                                ( relB
                                , inputs
                                , Just (readR mb relB inst b)
                                , inst)
                            return (Nothing, False)
                        else do
                            put (relB, inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just (ma, mb, _, 6), a, b, _) ->
                    if readR ma relB inst a == 0
                        then do
                            put
                                ( relB
                                , inputs
                                , Just (readR mb relB inst b)
                                , inst)
                            return (Nothing, False)
                        else do
                            put (relB, inputs, Just (i + 3), inst)
                            return (Nothing, False)
                (Just (ma, mb, mc, 7), a, b, c) ->
                    if readR ma relB inst a < readR mb relB inst b
                        then do
                            put
                                ( relB
                                , inputs
                                , Just (i + 4)
                                , writeR mc relB inst c 1)
                            return (Nothing, False)
                        else do
                            put
                                ( relB
                                , inputs
                                , Just (i + 4)
                                , writeR mc relB inst c 0)
                            return (Nothing, False)
                (Just (ma, mb, mc, 8), a, b, c) ->
                    if readR ma relB inst a == readR mb relB inst b
                        then do
                            put
                                ( relB
                                , inputs
                                , Just (i + 4)
                                , writeR mc relB inst c 1)
                            return (Nothing, False)
                        else do
                            put
                                ( relB
                                , inputs
                                , Just (i + 4)
                                , writeR mc relB inst c 0)
                            return (Nothing, False)
                (Just (ma, _, _, 9), a, _, _) -> do
                    put
                        ( relB + readR ma relB inst a
                        , inputs
                        , Just (i + 2)
                        , inst)
                    return (Nothing, False)
                (Just (_, _, _, 99), _, _, _) -> do
                    put (relB, inputs, Nothing, inst)
                    return (Nothing, True)
                (Just x, _, _, _) -> error $ show (x, i, inst)
                (_, _, _, _) -> error $ show (i, inst)

computer :: [Integer] -> Map Integer Integer -> _
computer inputs inst =
    last $
    catMaybes $
    fmap fst $
    flip evalState (0, inputs, Just 0, inst) $ unfoldWhileM (not . snd) step

answer1 :: _ -> _
answer1 = computer [1]

answer2 :: _ -> _
answer2 = computer [2]
