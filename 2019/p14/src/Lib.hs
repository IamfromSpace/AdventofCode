module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

import AdventOfCode.Util (applyNTimes)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split (splitOn)

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
       (alter, delete, foldr, fromList, keys, lookup, map, size)
import Data.Maybe (fromJust)
import Data.Monoid ()
import Data.Ratio (Ratio, (%))

import Data.Sequence ()
import Data.Set (Set)
import qualified Data.Set as Set
       (delete, fromList, insert, intersection)

parseChemCount :: _ -> (String, Integer)
parseChemCount s =
    let [count, chem] = splitOn " " s
    in (chem, read count)

parseLine :: String -> (String, (Integer, [(String, Integer)]))
parseLine s =
    let [a, b] = splitOn " => " s
        (name, get) = parseChemCount b
        inputs = fmap parseChemCount $ splitOn ", " a
    in (name, (get, inputs))

parse1 :: String -> Map String (Integer, [(String, Integer)])
parse1 = Map.fromList . fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

addInput ::
       Integer
    -> Integer
    -> (String, Integer)
    -> Map String Integer
    -> Map String Integer
addInput need get (chem, cost) lhs =
    let totalCost = ((need + get - 1) `div` get) * cost
    in Map.alter
           (\case
                Nothing -> Just totalCost
                Just currentCost -> Just (totalCost + currentCost))
           chem
           lhs

substitute ::
       Map String (Integer, [(String, Integer)])
    -> String
    -> Map String Integer
    -> Map String Integer
substitute costs toSub lhs =
    let need = fromJust $ Map.lookup toSub lhs
        lhs' = Map.delete toSub lhs
        (get, inputs) = fromJust $ Map.lookup toSub costs
    in foldr (addInput need get) lhs' inputs

getOreCost ::
       (Map String (Integer, [(String, Integer)]), Map String Integer)
    -> (Map String (Integer, [(String, Integer)]), Map String Integer)
getOreCost (costs, lhs) =
    let inputChemicals =
            Map.foldr
                (\(_, list) s -> foldr (\(c, _) s2 -> Set.insert c s2) s list)
                mempty
                costs
        neededChems = Set.fromList $ Map.keys lhs
        subable = foldr Set.delete neededChems inputChemicals
        costs' = foldr Map.delete costs subable
        lhs' = foldr (substitute costs) lhs subable
    in (costs', lhs')

answer1 :: Map String (Integer, [(String, Integer)]) -> _
answer1 costs =
    let toCount = "FUEL"
        lhs = Map.fromList $ snd $ fromJust $ Map.lookup toCount costs
       -- applyNTimes getOreCost (Map.delete toCount costs, lhs) 1
    in Map.lookup "ORE" $
       snd $
       until
           ((==) 1 . Map.size . snd)
           getOreCost
           (Map.delete toCount costs, lhs)

answer2 :: _ -> _
answer2 costs
    -- Dat manual binary search, lol
 =
    let guess = 0b001011111011010110111010
        toCount = "FUEL"
        lhs =
            Map.map ((*) guess) $
            Map.fromList $ snd $ fromJust $ Map.lookup toCount costs
       -- applyNTimes getOreCost (Map.delete toCount costs, lhs) 1
    in (,) guess $
       (\x -> x - 1000000000000) $
       fromJust $
       Map.lookup "ORE" $
       snd $
       until
           ((==) 1 . Map.size . snd)
           getOreCost
           (Map.delete toCount costs, lhs)
