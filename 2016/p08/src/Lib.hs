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

data Inst
    = Rect Int
           Int
    | RotateRow Int
                Int
    | RotateColumn Int
                   Int
    deriving (Show, Eq)

parseLine :: String -> Inst
parseLine x =
    case Split.splitOneOf " =" x of
        ["rect", d] ->
            let [a, b] = Split.splitOn "x" d
            in Rect (read a) (read b)
        ["rotate", "row", "y", y, "by", amount] ->
            RotateRow (read y) (read amount)
        ["rotate", "column", "x", x, "by", amount] ->
            RotateColumn (read x) (read amount)

parse1 :: String -> _
parse1 = fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

execute :: (Int, Int) -> Inst -> Set (Int, Int) -> Set (Int, Int)
execute (height, width) i screen =
    case i of
        Rect x y ->
            List.foldl' (flip Set.insert) screen $
            App.liftA2 (,) [0 .. x - 1] [0 .. y - 1]
        RotateRow row amount ->
            List.foldl'
                (\s x ->
                     let p = (x, row)
                         p' = ((x + amount) `mod` width, row)
                     in if Set.member p screen
                            then Set.insert p' s
                            else Set.delete p' s)
                screen $
            [0 .. width - 1]
        RotateColumn column amount ->
            List.foldl'
                (\s y ->
                     let p = (column, y)
                         p' = (column, (y + amount) `mod` height)
                     in if Set.member p screen
                            then Set.insert p' s
                            else Set.delete p' s)
                screen $
            [0 .. height - 1]

answer1 :: [Inst] -> Int
answer1 = Set.size . List.foldl' (flip (execute (6, 50))) mempty

answer2 :: [Inst] -> String
answer2 =
    Util.prettyPrintPointSetFlippable True '.' '#' .
    List.foldl' (flip (execute (6, 50))) mempty

show1 :: Show _a => _a -> String
show1 = show

show2 :: String -> String
show2 = id

ex1_1 :: Int
ex1_1 = 6

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
