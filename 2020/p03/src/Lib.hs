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

--parseGrid ::
--       (Integral a, Num a) => ((a, a) -> Char -> b -> b) -> b -> String -> b
parse1 :: String -> (Int, Int, Set (Int, Int))
parse1 s =
    let grid =
            Util.parseGrid
                (\p c m ->
                     if c == '#'
                         then Set.insert p m
                         else m)
                Set.empty
                s
        height = List.length (lines s)
        width = List.length (head (lines s))
    in (height, width, grid)

parse2 :: String -> _
parse2 = parse1

step :: (Int, Int, Set (Int, Int)) -> (Int, Int) -> (Int, Int) -> Int
step (h, w, g) (xxxx, yyyy) (x, y) =
    let p = ((x + xxxx) `mod` w, y + yyyy)
        trees =
            if Set.member p g
                then 1
                else 0
    in if y + 1 == h
           then trees
           else trees + step (h, w, g) (xxxx, yyyy) p

answer1 :: _ -> _
answer1 x = step x (3, 1) (0, 0)

answer2 :: _ -> _
answer2 i =
    List.foldl' (*) 1 $
    fmap
        (\(x, y) -> step i (x, y) (0, 0))
        [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

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
