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

parse1 :: String -> [Int]
parse1 = fmap read . lines

parse2 :: String -> _
parse2 = parse1

combos :: [Int] -> [(Int, Int)]
combos is = do
    i <- is
    j <- is
    return (i, j)

answer1 :: _ -> _
answer1 = fmap (\(a, b) -> a * b) . filter (\(a, b) -> a + b == 2020) . combos

combos2 :: [Int] -> [(Int, Int, Int)]
combos2 is = do
    i <- is
    j <- is
    k <- is
    return (i, j, k)

answer2 :: _ -> _
answer2 =
    fmap (\(a, b, c) -> a * b * c) .
    filter (\(a, b, c) -> a + b + c == 2020) . combos2

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: [Int]
ex1_1 = [51457]

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
