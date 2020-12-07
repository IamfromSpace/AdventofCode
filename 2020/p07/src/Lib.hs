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

parseBagCounts :: String -> (Int, String)
parseBagCounts x =
    let (h, t) = break ((==) ' ') x
        (h':_) = Split.splitOn " bag" (drop 1 t)
    in (read h, h')

parseLine :: String -> Maybe (String, [(Int, String)])
parseLine s =
    let [s', _] = Split.splitOn "." s
        [x, xs] = Split.splitOn " bags contain " s'
    in if xs == "no other bags"
           then Nothing
           else Just (x, fmap parseBagCounts (Split.splitOn ", " xs))

parse1 :: String -> Map String [(Int, String)]
parse1 = Map.fromList . Maybe.catMaybes . fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

canHold :: Map String [(Int, String)] -> String -> String -> Bool
canHold m target b =
    Maybe.fromMaybe False $
    any ((\x -> x == target || canHold m target x) . snd) <$> Map.lookup b m

answer1 :: _ -> _
answer1 m = List.length $ filter (canHold m "shiny gold") $ Map.keys m

countContents :: Map String [(Int, String)] -> String -> Int
countContents m b =
    Maybe.fromMaybe 0 $
    (sum . fmap (\(count, x) -> count * (1 + countContents m x))) <$>
    Map.lookup b m

answer2 :: Map String [(Int, String)] -> Int
answer2 = flip countContents "shiny gold"

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: Int
ex1_1 = 4

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: Int
ex2_1 = 126

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
