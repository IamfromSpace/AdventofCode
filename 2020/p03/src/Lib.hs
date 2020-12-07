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

parse1 :: String -> _
parse1 = fmap (fmap ((==) '#')) . lines

parse2 :: String -> _
parse2 = parse1

skip :: Int -> [a] -> [a]
skip n (h:t) = h : skip n (List.drop n t)
skip _ [] = []

slope :: (Int, Int) -> _ -> Int
slope (x, y) xs =
    let width = List.length (head xs)
    in List.length $
       List.filter id $
       List.zipWith (!!) (skip (y - 1) xs) (fmap (\x -> x `mod` width) [0,x ..])

answer1 :: _ -> Int
answer1 = slope (3, 1)

answer2 :: _ -> _
answer2 xs =
    List.product $
    fmap (\p -> slope p xs) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: Int
ex1_1 = 7

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
