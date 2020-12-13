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
import Data.Foldable (minimumBy, toList)
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

parseBus :: String -> Maybe Int
parseBus s =
    case s of
        "x" -> Nothing
        x -> Just $ read x

parse1 :: String -> (Int, [Int])
parse1 xs =
    let [start, busses] = lines xs
    in (read start, Maybe.catMaybes $ fmap parseBus $ Split.splitOn "," busses)

parse2 :: String -> [(Int, Int)]
parse2 xs =
    let [_, busses] = lines xs
    in Util.elmTrace $
       Maybe.catMaybes $
       List.zipWith (\i -> fmap ((,) i)) [0 ..] $
       fmap parseBus $ Split.splitOn "," busses

earliest :: Int -> Int -> Int
earliest start time =
    getSum $
    head $
    List.dropWhile ((\x -> x < start) . getSum) $
    fmap (flip stimes (Sum time)) [1 ..]

answer1 :: (Int, [_]) -> _
answer1 (start, busses) =
    let (time, id) =
            head $ List.sort $ fmap (\x -> (earliest start x, x)) busses
    in (time - start) * id

test :: Int -> [(Int, Int)] -> Bool
test t = all (\(min, bus) -> (t + min) `mod` bus == 0)

sync :: Int -> (Int, Int) -> (Int, Int)
sync bus1 (min2, bus2) =
    let [a, b] =
            fmap ((*) bus1) $
            take 2 $ filter (\n -> (n * bus1 + min2) `mod` bus2 == 0) [1 ..]
    in (a, b - a)

-- (t `mod` bus) == ((t + min) `mod` bus2)
-- total hack, couldn't figure out how to combine larger cycles, so this was
-- the biggest one I could correctly identify.  Solves in about 30min
answer2 :: _ -> _
answer2 xs = head $ filter (flip test xs) [14934,14934 + 14953 ..]

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

ex2_2 :: Int
ex2_2 = 3417

ex2_3 :: Int
ex2_3 = 754018

ex2_4 :: Int
ex2_4 = 779210

ex2_5 :: Int
ex2_5 = 1261476
