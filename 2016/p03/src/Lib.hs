module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util ()
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Data.List ()
import Data.List.Split ()
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

type Parsed1 = [(Integer, Integer, Integer)]

type Parsed2 = Parsed1

type Answer1 = Int

type Answer2 = Int

parseLine :: String -> (Integer, Integer, Integer)
parseLine x =
    let [a, b, c] = fmap read $ words x
    in (a, b, c)

isValidTriangle :: (Integer, Integer, Integer) -> Bool
isValidTriangle (a, b, c) = a + b > c && a + c > b && b + c > a

parse1 :: String -> Parsed1
parse1 = fmap parseLine . lines

parse2 :: String -> Parsed2
parse2 = parse1

answer1 :: Parsed1 -> Answer1
answer1 = length . filter isValidTriangle

reorganize :: Parsed1 -> Parsed1
reorganize [] = []
reorganize ((a0, a1, a2):(b0, b1, b2):(c0, c1, c2):t) =
    (a0, b0, c0) : (a1, b1, c1) : (a2, b2, c2) : reorganize t

answer2 :: Parsed2 -> Answer2
answer2 = answer1 . reorganize
