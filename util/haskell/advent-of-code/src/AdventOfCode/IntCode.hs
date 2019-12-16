module AdventOfCode.IntCode
    ( parseInsts
    , step
    , consume
    , WaitState(..)
    , Computer
    , Insts
    , initialize
    , stepN
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)

listToIndexMap :: [a] -> Map Integer a
listToIndexMap =
    fst . foldl (\(m, i) v -> (Map.insert i v m, i + 1)) (mempty, 0)

parseInsts :: String -> Insts
parseInsts = listToIndexMap . fmap read . splitOn ","

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

data WaitState
    = Halt
    | Prompt
    deriving (Eq, Show)

type Insts = Map Integer Integer

-- ( Relative Offset
-- , Internal Input Buffer
-- , PC (Nothing if Halted)
-- , Self-Modifying Instruction Set
-- )
type Computer = (Integer, Seq Integer, Maybe Integer, Map Integer Integer)

initialize :: Map Integer Integer -> Computer
initialize m = (0, mempty, Just 0, m)

-- (StartState and Maybe NewInputs) ->
-- (NextState and Left means stop, Right means continue)
step ::
       (Computer, Maybe (Seq Integer))
    -> (Computer, Either WaitState (Maybe Integer))
step (s@(_, _, Nothing, _), Just _)
  -- If we got an input, but the program was halted, drop the input and report Halt
 = (s, Left Halt)
step ((relB, inputs, pc, insts), Just newInputs)
  -- Otherwise, if we got an input, bring it into the buffer and step again
 = step ((relB, inputs <> newInputs, pc, insts), mempty)
step (state, Nothing)
  -- If we did not get an input and are not halted (attempt), proceed with the next instruction
 =
    case state of
        (_, _, Nothing, _) -> (state, Left Halt)
        (relB, inputs, Just i, insts) ->
            case ( decodeOp <$> Map.lookup i insts
                 , fromMaybe 0 $ Map.lookup (i + 1) insts
                 , fromMaybe 0 $ Map.lookup (i + 2) insts
                 , fromMaybe 0 $ Map.lookup (i + 3) insts) of
                (Just (ma, mb, mc, 1), a, b, c) ->
                    ( ( relB
                      , inputs
                      , Just (i + 4)
                      , writeR
                            mc
                            relB
                            insts
                            c
                            (readR ma relB insts a + readR mb relB insts b))
                    , Right Nothing)
                (Just (ma, mb, mc, 2), a, b, c) ->
                    ( ( relB
                      , inputs
                      , Just (i + 4)
                      , writeR
                            mc
                            relB
                            insts
                            c
                            (readR ma relB insts a * readR mb relB insts b))
                    , Right Nothing)
                (Just (ma, _, _, 3), a, _, _) ->
                    case inputs of
                        Empty -> (state, Left Prompt)
                        (h :<| t) ->
                            ( (relB, t, Just (i + 2), writeR ma relB insts a h)
                            , Right Nothing)
                (Just (ma, _, _, 4), a, _, _) ->
                    ( (relB, inputs, Just (i + 2), insts)
                    , Right (Just (readR ma relB insts a)))
                (Just (ma, mb, _, 5), a, b, _) ->
                    (\s -> (s, Right Nothing)) $
                    if readR ma relB insts a /= 0
                        then (relB, inputs, Just (readR mb relB insts b), insts)
                        else (relB, inputs, Just (i + 3), insts)
                (Just (ma, mb, _, 6), a, b, _) ->
                    (\s -> (s, Right Nothing)) $
                    if readR ma relB insts a == 0
                        then (relB, inputs, Just (readR mb relB insts b), insts)
                        else (relB, inputs, Just (i + 3), insts)
                (Just (ma, mb, mc, 7), a, b, c) ->
                    (\s -> (s, Right Nothing)) $
                    if readR ma relB insts a < readR mb relB insts b
                        then ( relB
                             , inputs
                             , Just (i + 4)
                             , writeR mc relB insts c 1)
                        else ( relB
                             , inputs
                             , Just (i + 4)
                             , writeR mc relB insts c 0)
                (Just (ma, mb, mc, 8), a, b, c) ->
                    (\s -> (s, Right Nothing)) $
                    if readR ma relB insts a == readR mb relB insts b
                        then ( relB
                             , inputs
                             , Just (i + 4)
                             , writeR mc relB insts c 1)
                        else ( relB
                             , inputs
                             , Just (i + 4)
                             , writeR mc relB insts c 0)
                (Just (ma, _, _, 9), a, _, _) ->
                    ( ( relB + readR ma relB insts a
                      , inputs
                      , Just (i + 2)
                      , insts)
                    , Right Nothing)
                (Just (_, _, _, 99), _, _, _) ->
                    ((relB, inputs, Nothing, insts), Left Halt)
                (Just x, _, _, _) -> error $ show (x, i, insts)
                (_, _, _, _) -> error $ show (i, insts)

consume' ::
       Seq Integer
    -> (Computer, Seq Integer)
    -> (Computer, (WaitState, Seq Integer))
consume' outs (state, inputs) =
    case step (state, Just inputs) of
        (nextState, Left r) -> (nextState, (r, outs))
        (nextState, Right mOut) ->
            consume'
              -- If we got an output, append it
                (maybe outs (\out -> outs |> out) mOut)
                (nextState, mempty)

consume :: (Computer, Seq Integer) -> (Computer, (WaitState, Seq Integer))
consume = consume' mempty

stepN' ::
       Seq Integer
    -> Integer
    -> (Computer, Seq Integer)
    -> (Computer, (Maybe WaitState, Seq Integer))
stepN' outs 0 (state, inputs) = (state, (Nothing, outs))
stepN' outs n (state, inputs) =
    case step (state, Just inputs) of
        (nextState, Left r) -> (nextState, (Just r, outs))
        (nextState, Right mOut) ->
            stepN'
              -- If we got an output, append it
                (maybe outs (\out -> outs |> out) mOut)
                (n - 1)
                (nextState, mempty)

stepN ::
       Integer
    -> (Computer, Seq Integer)
    -> (Computer, (Maybe WaitState, Seq Integer))
stepN = stepN' mempty
