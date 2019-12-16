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
--Note:  Modified from original solve to use the IntCode utility
import AdventOfCode.IntCode
       (Computer, Insts, WaitState(..), consume, initialize, parseInsts)
import AdventOfCode.Util (elmTrace)
import Control.Applicative ((<*>))
import Control.Monad (replicateM)
import Control.Monad.Loops (unfoldWhileM)
import Control.Monad.State.Lazy
       (State, evalState, get, put, runState)
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import Data.List (permutations)
import Data.List.Split (splitOn)
import qualified Data.Map as Map ()
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (fromList, lookup, update)
import qualified Data.Set as Set ()

parse1 :: String -> _
parse1 = parseInsts

parse2 :: String -> _
parse2 = parse1

amps :: _ -> (Integer, Integer, Integer, Integer, Integer) -> _
amps insts (a, b, c, d, e) =
    let ini = initialize insts
        (_, (_, aout)) = consume (ini, pure a <> pure 0)
        (_, (_, bout)) = consume (ini, pure b <> aout)
        (_, (_, cout)) = consume (ini, pure c <> bout)
        (_, (_, dout)) = consume (ini, pure d <> cout)
        (_, (_, eout)) = consume (ini, pure e <> dout)
    in last $ toList eout

answer1 :: _ -> _
answer1 inst =
    maximum $
    fmap (\x -> (amps inst x, x)) $
    fmap (\[a, b, c, d, e] -> (a, b, c, d, e)) $ permutations [0 .. 4]

setup ::
       Insts
    -> (Integer, Integer, Integer, Integer, Integer)
    -> (Computer, Computer, Computer, Computer, Computer)
setup insts (a, b, c, d, e) =
    let ini = initialize insts
        -- We ignore outputs, because we know it
        -- that it prompts immediately after accepting Parameters
        (as, _) = consume (ini, pure a)
        (bs, _) = consume (ini, pure b)
        (cs, _) = consume (ini, pure c)
        (ds, _) = consume (ini, pure d)
        (es, _) = consume (ini, pure e)
    in (as, bs, cs, ds, es)

amps2 ::
       ((Computer, Computer, Computer, Computer, Computer), Seq Integer)
    -> Integer
amps2 ((as, bs, cs, ds, es), firstIn) =
    let (as', (_, aout)) = consume (as, firstIn)
        (bs', (_, bout)) = consume (bs, aout)
        (cs', (_, cout)) = consume (cs, bout)
        (ds', (_, dout)) = consume (ds, cout)
    in case consume (es, dout) of
           (es', (Prompt, eout)) -> amps2 ((as', bs', cs', ds', es'), eout)
           (_, (Halt, eout)) -> last $ toList eout

answer2 :: _ -> _
answer2 inst =
    maximum $
    fmap (\x -> amps2 (setup inst x, pure 0)) $
    fmap (\[a, b, c, d, e] -> (a, b, c, d, e)) $ permutations [5 .. 9]
