module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (|||), arr)
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq(..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Prelude hiding ((++), init, lookup, map)
import Text.ParserCombinators.PArrow (MD, (>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.Read (readMaybe)

parse1 :: String -> [Int]
parse1 = fmap read . lines

parse2 :: String -> _
parse2 = parse1

answer1 :: [Int] -> _
answer1 xs =
    let pairs =
            filter (\x -> List.length x == 2) $
            fmap (take 2) $ List.tails $ List.sort xs
        ones = List.length $ filter (\[a, b] -> b - a == 1) pairs
        threes = List.length $ filter (\[a, b] -> b - a == 3) pairs
    in (ones + 1) * (threes + 1)

combos :: Int -> Int
combos 0 = 1
combos 1 = 1
combos 2 = 2
combos 3 = 4
combos 4 = 7

answer2 :: [Int] -> Int
answer2 xs =
    product $
    fmap (combos . List.length) $
    Split.splitWhen ((==) 3) $
    fmap (\[a, b] -> b - a) $
    filter (\x -> List.length x == 2) $
    fmap (take 2) $ List.tails $ List.sort (0 : xs)

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: Int
ex1_1 = 220

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: Int
ex2_1 = 8

ex2_2 :: Int
ex2_2 = 19208

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
