module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , genPoints
    ) where

import AdventOfCode.IntCode
       (Computer, consume, initialize, parseInsts)

import AdventOfCode.Util (elmTrace, prettyPrintPointSet)
import Control.Applicative (liftA2)
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import Data.List ()
import Data.List.Split ()
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace (trace)

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

parse1 :: String -> _
parse1 = parseInsts

parse2 :: String -> _
parse2 = parse1

sumPoints :: Integer -> [(Integer, Integer)] -> Computer -> Integer
sumPoints count [] _ = count
sumPoints count ((x, y):t) c =
    let (s, (_, r)) = consume (c, Seq.fromList [x, y])
    in sumPoints (head (toList r) + count) t c

answer1 :: _ -> _
answer1 insts = sumPoints 0 (liftA2 (,) [0 .. 49] [0 .. 49]) (initialize insts)

makeSet ::
       Set (Integer, Integer)
    -> [(Integer, Integer)]
    -> Computer
    -> Set (Integer, Integer)
makeSet s [] _ = s
makeSet s ((x, y):t) c =
    let (_, (_, r)) = consume (c, Seq.fromList [x, y])
    in makeSet
           (if 1 == head (toList r)
                then Set.insert (x, y) s
                else s)
           t
           c

check100x100 :: Set (Integer, Integer) -> (Integer, Integer) -> Bool
check100x100 s (x, y) =
    all (flip Set.member s) $ liftA2 (,) [x .. x + 99] [y .. y + 99]

genPoints :: Integer -> [(Integer, Integer)]
genPoints d =
    (d, d) : liftA2 (,) [0 .. d - 1] [d] ++ liftA2 (,) [d] [0 .. d - 1]

start :: Computer -> Set (Integer, Integer)
start c = flip (makeSet mempty) c $ concatMap genPoints [0 .. 99]

search :: Computer -> Set (Integer, Integer) -> Integer -> (Integer, Integer)
search c s i =
    case filter (check100x100 s) $ genPoints i of
        [p] -> p
        _ ->
            let newSet = makeSet s (genPoints (i + 100)) c
            in search
                   c
                   --(trace (prettyPrintPointSet ' ' '#' newSet) newSet)
                   newSet
                   (i + 1)

answer2 :: _ -> _
answer2 insts
    --trace (prettyPrintPointSet ' ' '#' $ start (initialize insts)) insts
 =
    let c = initialize insts
    in search c (start c) 0
