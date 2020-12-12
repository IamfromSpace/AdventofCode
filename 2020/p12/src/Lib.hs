module Lib where

import AdventOfCode.Util (Vector(..), manLen, multiLines)
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
import Data.Semigroup (stimes)
import Data.Sequence (Seq(..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Prelude hiding ((++), init, lookup, map)
import Text.ParserCombinators.PArrow (MD, (>>!))
import qualified Text.ParserCombinators.PArrow as PA
import Text.Read (readMaybe)

data Inst
    = V (Vector (Integer, Integer))
    | Turn Integer
    | Forward Integer
    deriving (Show)

parseV :: String -> Inst
parseV s =
    case head $ take 1 s of
        'N' -> V $ Vector (0, read (drop 1 s))
        'S' -> V $ Vector (0, -1 * read (drop 1 s))
        'E' -> V $ Vector (read (drop 1 s), 0)
        'W' -> V $ Vector (-1 * read (drop 1 s), 0)
        'R' -> Turn $ ((read (drop 1 s) `div` 90) `mod` 4)
        'L' -> Turn $ (4 - (read (drop 1 s) `div` 90) `mod` 4)
        'F' -> Forward $ read (drop 1 s)

parse1 :: String -> [Inst]
parse1 = fmap parseV . lines

parse2 :: String -> _
parse2 = parse1

step ::
       Inst
    -> (Vector (Integer, Integer), Integer)
    -> (Vector (Integer, Integer), Integer)
step (V a) (v, f) = (a <> v, f)
step (Turn x) (v, f) = (v, (f + x) `mod` 4)
step (Forward x) (v, 0) = (v <> Vector (x, 0), 0)
step (Forward x) (v, 1) = (v <> Vector (0, -x), 1)
step (Forward x) (v, 2) = (v <> Vector (-x, 0), 2)
step (Forward x) (v, 3) = (v <> Vector (0, x), 3)

answer1 :: [Inst] -> _
answer1 = manLen . fst . List.foldl' (flip step) (Vector (0, 0), 0)

sub :: Vector (Integer, Integer)
    -> Vector (Integer, Integer)
    -> Vector (Integer, Integer)
Vector (x1, y1) `sub` Vector (x2, y2) = Vector (x2 - x1, y2 - y1)

step2 ::
       Inst
    -> (Vector (Integer, Integer), Vector (Integer, Integer))
    -> (Vector (Integer, Integer), Vector (Integer, Integer))
step2 (V a) (ship, waypoint) = (ship, waypoint <> a)
step2 (Forward x) (ship, waypoint) = (ship <> stimes x waypoint, waypoint)
step2 (Turn 0) (ship, waypoint) = (ship, waypoint)
step2 (Turn 1) (ship, Vector (x, y)) = (ship, Vector (y, -x))
step2 (Turn 2) (ship, Vector (x, y)) = (ship, Vector (-x, -y))
step2 (Turn 3) (ship, Vector (x, y)) = (ship, Vector (-y, x))

answer2 :: [Inst] -> _
answer2 =
    manLen . fst . List.foldl' (flip step2) (Vector (0, 0), Vector (10, 1))

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

ex2_1 :: Integer
ex2_1 = 286

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
