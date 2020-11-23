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
import Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding ((++), lookup, map)

parse1 :: String -> _
parse1 = id

parse2 :: String -> _
parse2 = parse1

breakAt :: Char -> String -> (String, String)
breakAt c s = breakAt' c ("", s)

breakAt' :: Char -> (String, String) -> (String, String)
breakAt' c (taken, h:t) =
    if h == c
        then (taken, t)
    -- TODO: inefficient
        else breakAt' c (taken <> [h], t)
breakAt' _ x = x

expand :: String -> String
expand ('(':t) =
    let (charCountS, rem0) = breakAt 'x' t
        (repsS, rem1) = breakAt ')' rem0
        charCount = read charCountS
        reps = read repsS
        repSeq = List.take charCount rem1
        rem2 = List.drop charCount rem1
    in (List.concat (List.replicate reps repSeq)) <> expand rem2
expand (h:t) = h : expand t
expand [] = []

-- 25:46 -> 42:11
answer1 :: _ -> _
answer1 = List.length . expand

recExpand :: String -> Int
recExpand ('(':t) =
    let (charCountS, rem0) = breakAt 'x' t
        (repsS, rem1) = breakAt ')' rem0
        charCount = read charCountS
        reps = read repsS
        repSeq = List.take charCount rem1
        rem2 = List.drop charCount rem1
    in reps * (recExpand repSeq) + recExpand rem2
recExpand (_:t) = 1 + recExpand t
recExpand [] = 0

-- 49:37 - This would have been 63 on the leaderboard!
answer2 :: _ -> _
answer2 = recExpand

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
