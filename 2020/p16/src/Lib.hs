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
import Data.Foldable (toList)
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

parseRange :: String -> [Int]
parseRange s =
    let [a, b] = Split.splitOn "-" s
    in [read a .. read b]

parseRule :: String -> (String, ([Int], [Int]))
parseRule s =
    let [name, rem] = Split.splitOn ": " s
        [one, two] = Split.splitOn " or " rem
    in (name, (parseRange one, parseRange two))

parseTicket :: String -> [Int]
parseTicket = fmap read . Split.splitOn ","

parse1 :: String -> ([(String, ([Int], [Int]))], [Int], [[Int]])
parse1 s =
    let [rules, [_, myTicket], (_:nearby)] = multiLines s
    in (fmap parseRule $ rules, parseTicket myTicket, fmap parseTicket nearby)

parseRule2 :: String -> (String, Set Int)
parseRule2 s =
    let [name, rem] = Split.splitOn ": " s
        [one, two] = Split.splitOn " or " rem
    in ( name
       , ((Set.fromList $ parseRange one) <> (Set.fromList $ parseRange two)))

parse2 :: String -> ([(String, Set Int)], [Int], [[Int]])
parse2 s =
    let [rules, [_, myTicket], (_:nearby)] = multiLines s
    in (fmap parseRule2 $ rules, parseTicket myTicket, fmap parseTicket nearby)

combineRules :: [(String, ([Int], [Int]))] -> [[Int]]
combineRules xs = List.concat $ fmap (\(a, b) -> [a, b]) $ fmap snd xs

checkTicket :: [[Int]] -> [Int] -> [Int]
checkTicket rules ticket =
    filter (not . (\n -> any (any ((==) n)) rules)) ticket

answer1 :: _ -> _
answer1 (rules, _, nearby) =
    let r = combineRules rules
    in sum $ (nearby >>= checkTicket r)

checkTicket2 :: [Set Int] -> [Int] -> Bool
checkTicket2 !rules !ticket = all (\n -> any (Set.member n) rules) ticket

validForColumn :: Set Int -> [Int] -> Bool
validForColumn !ruleRanges !xs = all (flip Set.member ruleRanges) xs

validColumnIds' :: Int -> Set Int -> [[Int]] -> [Int]
validColumnIds' !i !ruleRanges ((!h):(!t)) =
    (if validForColumn ruleRanges h
         then (:) i
         else id) $
    validColumnIds' (i + 1) ruleRanges t
validColumnIds' _ _ [] = []

validColumnIds :: Set Int -> [[Int]] -> [Int]
validColumnIds = validColumnIds' 0

toValidCombos :: [Set Int] -> [[Int]] -> [[Int]]
toValidCombos !rulesRanges !columns =
    fmap (flip validColumnIds columns) rulesRanges

inputToValidCombos :: ([(String, Set Int)], [Int], [[Int]]) -> [[Int]]
inputToValidCombos (rules, _, nearby) =
    toValidCombos (fmap snd rules) (List.transpose nearby)

-- left as is, but solved this by hand based on in a REPL:
-- @
-- inputToValidCombos (fields, x, filter (checkTicket2 (fmap snd fields)) nearby)
-- @
--
-- scratch includes the by hand result of finding the valid permutation from
-- that result
answer2 :: ([(String, Set Int)], [Int], [[Int]]) -> _
answer2 (fields, _, nearby) =
    let columns =
            List.transpose $ filter (checkTicket2 (fmap snd fields)) nearby
        order =
            head $
            filter
                (\ordering ->
                     all id $
                     List.zipWith
                         (\(_, valid) is -> all (flip Set.member valid) is)
                         ordering
                         columns)
                (List.permutations fields)
    in fmap fst $ order

{-
    head $
    filter ((==) (List.length fields) . Set.size . Set.fromList) $
    sequence $
    inputToValidCombos
        (fields, x, filter (checkTicket2 (fmap snd fields)) nearby)
-}
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

ex2_1 :: [String]
ex2_1 = ["row", "class", "seat"]

ex2_2 :: [String]
ex2_2 = ["row", "class", "seat"]

--ex2_1 :: [Int]
--ex2_1 = [1, 0, 2]
--ex2_2 :: [Int]
--ex2_2 = [1, 0, 2]
ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
