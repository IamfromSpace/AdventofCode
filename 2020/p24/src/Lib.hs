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
import Data.Foldable (fold, foldMap, toList)
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

data Dir
    = E
    | SE
    | SW
    | W
    | NW
    | NE
    deriving (Show, Eq)

parseLine :: String -> [Dir]
parseLine ('s':'e':t) = SE : parseLine t
parseLine ('s':'w':t) = SW : parseLine t
parseLine ('n':'e':t) = NE : parseLine t
parseLine ('n':'w':t) = NW : parseLine t
parseLine ('e':t) = E : parseLine t
parseLine ('w':t) = W : parseLine t
parseLine [] = []

parse1 :: String -> _
parse1 = fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

toVec :: Dir -> Vector (Integer, Integer, Integer)
toVec E = Vector (1, -1, 0)
toVec SE = Vector (0, -1, 1)
toVec SW = Vector (-1, 0, 1)
toVec W = Vector (-1, 1, 0)
toVec NW = Vector (0, 1, -1)
toVec NE = Vector (1, 0, -1)

flipIfOrTrue :: Maybe Bool -> Maybe Bool
flipIfOrTrue Nothing = Just True
flipIfOrTrue (Just True) = Just False
flipIfOrTrue (Just False) = Just True

toMap ::
       Map (Vector (Integer, Integer, Integer)) Bool
    -> [[Dir]]
    -> Map (Vector (Integer, Integer, Integer)) Bool
toMap !m (h:t) = toMap (Map.alter flipIfOrTrue (foldMap toVec h) m) t
toMap !m [] = m

answer1 :: [[Dir]] -> _
answer1 = length . filter id . fmap snd . Map.toList . toMap mempty

neighbors :: [Vector (Integer, Integer, Integer)]
neighbors = fmap toVec [E, SE, SW, W, NW, NE]

poi :: [Vector (Integer, Integer, Integer)]
    -> [Vector (Integer, Integer, Integer)]
poi vs =
    Set.toList $
    Set.fromList $ do
        v <- vs
        n <- neighbors
        return (v <> n)

tileDay ::
       Map (Vector (Integer, Integer, Integer)) Bool
    -> ((Vector (Integer, Integer, Integer)), Bool)
    -> Bool
tileDay m (v, isBlack) =
    let surroundingBlackCount =
            length $
            filter id $
            Maybe.catMaybes $ fmap (flip Map.lookup m . (<>) v) neighbors
    in if isBlack
           then surroundingBlackCount == 1 || surroundingBlackCount == 2
           else surroundingBlackCount == 2

day :: Map (Vector (Integer, Integer, Integer)) Bool
    -> Map (Vector (Integer, Integer, Integer)) Bool
day m =
    Map.fromList $
    fmap (\t -> (t, tileDay m (t, Maybe.fromMaybe False $ Map.lookup t m))) $
    poi $ Map.keys m

answer2 :: [[Dir]] -> _
answer2 ds =
    length $
    filter id $ fmap snd $ Map.toList $ ((iterate day (toMap mempty ds)) !! 100)

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
