module Lib where

import AdventOfCode.Util (Vector(..), multiLines)
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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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

input :: MD String String
input = PA.many PA.anyChar

--parseGrid ::
--       (Integral a, Num a) => ((a, a) -> Char -> b -> b) -> b -> String -> b
parse1 :: String -> Map (Vector (Integer, Integer)) Bool
parse1 =
    Util.parseGrid
        (\p c m ->
             if c == 'L'
                 then Map.insert (Vector p) False m
                 else m)
        Map.empty

data State
    = Taken
    | Free
    | Floor
    deriving (Eq, Show)

parse2 :: String -> Map (Vector (Integer, Integer)) State
parse2 =
    Util.parseGrid
        (\p c m ->
             case c of
                 'L' -> Map.insert (Vector p) Free m
                 '#' -> Map.insert (Vector p) Taken m
                 '.' -> Map.insert (Vector p) Floor m
                 _ -> error "bad input")
        Map.empty

neighbors :: Num a => [Vector (a, a)]
neighbors = take 8 $ App.liftA2 (\x y -> Vector (x, y)) [-1, 1, 0] [-1, 1, 0]

step ::
       Map (Vector (Integer, Integer)) Bool
    -> Map (Vector (Integer, Integer)) Bool
step m =
    Map.foldrWithKey
        (\p currentlyOcc m' ->
             let occCount =
                     List.length $
                     filter id $
                     Maybe.catMaybes $
                     fmap (flip Map.lookup m . ((<>) p)) neighbors
                 nextState =
                     if currentlyOcc
                         then not (occCount >= 4)
                         else occCount == 0
             in Map.insert p nextState m')
        mempty
        m

answer1 :: _ -> _
answer1 = List.length . List.filter id . Map.elems . fix step

mappendForever :: Monoid a => a -> a -> [a]
mappendForever offset init =
    let x = (init <> offset)
    in x : mappendForever offset x

look ::
       Map (Vector (Integer, Integer)) State
    -> Vector (Integer, Integer)
    -> Vector (Integer, Integer)
    -> Bool
look m p offset =
    let takenSeats =
            List.filter ((/=) (Just Floor)) $
            List.takeWhile Maybe.isJust $
            fmap (flip Map.lookup m) $ mappendForever p offset
    in case takenSeats of
           (Just Taken:_) -> True
           _ -> False

getNextState ::
       Map (Vector (Integer, Integer)) State
    -> Vector (Integer, Integer)
    -> State
    -> State
getNextState m p state =
    let occCount = List.length $ filter id $ fmap (look m p) $ neighbors
    in case state of
           Floor -> Floor
           Taken ->
               if occCount >= 5
                   then Free
                   else Taken
           Free ->
               if occCount == 0
                   then Taken
                   else Free

step2 ::
       Map (Vector (Integer, Integer)) State
    -> Map (Vector (Integer, Integer)) State
step2 m = Map.foldrWithKey (\p -> Map.insert p . getNextState m p) mempty m

fix :: Eq a => (a -> a) -> a -> a
fix fn a =
    let next = fn a
    in if a == next
           then a
           else fix fn next

answer2 :: _ -> _
answer2 = List.length . List.filter ((==) Taken) . Map.elems . fix step2

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

ex2_1 :: Int
ex2_1 = 26

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
