module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
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
import Text.Read (readMaybe)

to10 :: Char -> Int
to10 'L' = 0
to10 'B' = 1
to10 'R' = 1
to10 'F' = 0

parse1 :: String -> [(String, String)]
parse1 = fmap (\x -> (take 7 x, drop 7 x)) . lines

parse2 :: String -> _
parse2 = parse1

toNum :: String -> Int
toNum x =
    sum $
    reverse $ List.zipWith (\a b -> a * (2 ^ b)) (reverse (fmap to10 x)) [0 ..]

toNums :: (String, String) -> (Int, Int)
toNums (row, col) = (toNum row, toNum col)

toId = (\(row, col) -> row * 8 + col)

answer1 :: [(String, String)] -> _
answer1 xs = List.maximum $ fmap (toId . toNums) xs

allSeats :: Set (Int, Int)
allSeats = Set.fromList $ App.liftA2 (,) [0 .. 2 ^ 7 - 1] [0 .. 2 ^ 3 - 1]

answer2 :: [(String, String)] -> _
answer2 xs =
    filter
        (\case
             [a, b] -> b - a /= 1
             _ -> False) $
    fmap (take 2) $
    List.tails $
    fmap (toId) $
    Set.toList $ List.foldl' (flip Set.delete) allSeats (fmap toNums xs)

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
