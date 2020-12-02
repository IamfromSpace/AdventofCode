module Lib where

import AdventOfCode.Util ()
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Prelude hiding ((++), init, lookup, map)

data Policy =
    Policy Char
           Int
           Int
    deriving (Show, Eq)

parseLine :: String -> (Policy, String)
parseLine s =
    let [policy, pass] = Split.splitOn ": " s
        [low, high, [char]] = Split.splitOneOf "- " policy
    in (Policy char (read low) (read high), pass)

parse1 :: String -> _
parse1 = fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

testPolicy :: Policy -> String -> Bool
testPolicy (Policy c low high) pass =
    let count = List.length $ List.filter ((==) c) pass
    in count >= low && count <= high

answer1 :: _ -> _
answer1 = List.length . List.filter (uncurry testPolicy)

testPolicy2 :: Policy -> String -> Bool
testPolicy2 (Policy c low high) pass =
    let (l:rem) = List.drop (low - 1) pass
        (h:_) = List.drop (high - low - 1) rem
    in (l == c) /= (h == c)

answer2 :: _ -> _
answer2 = List.length . List.filter (uncurry testPolicy2)

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
