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

parseLine :: String -> [(String, String)]
parseLine s =
    let ts = Split.splitOn " " (s)
    in fmap
           ((\case
                 [a, b] -> (a, b)
                 x -> error (show x)) .
            Split.splitOn ":")
           ts

parseLines :: [[(String, String)]] -> [String] -> [[(String, String)]]
parseLines xs [] = xs
parseLines xs ([]:t) = parseLines ([] : xs) t
parseLines (x:xs) (h:t) = parseLines ((parseLine h <> x) : xs) t

parse1 :: String -> [Map String String]
parse1 = fmap (Map.fromList) . parseLines [[]] . lines

parse2 :: String -> _
parse2 = parse1

validate :: Map String String -> Bool
validate m =
    all
        (flip Set.member (Set.fromList $ Map.keys m))
        ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- no cid?

answer1 :: _ -> _
answer1 = List.length . filter validate

validate2 :: Map String String -> Bool
validate2 m =
    let hasAll =
            all
                (flip Set.member (Set.fromList $ Map.keys m))
                ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- no cid?
        byr =
            maybe False ((\x -> x >= 1920 && x <= 2002) . read) $
            Map.lookup "byr" m
        iyr =
            maybe False ((\x -> x >= 2010 && x <= 2020) . read) $
            Map.lookup "iyr" m
        eyr =
            maybe False ((\x -> x >= 2020 && x <= 2030) . read) $
            Map.lookup "eyr" m
        hgt =
            maybe
                False
                (((\case
                       'm':'c':x ->
                           let cm = read (reverse x)
                           in cm >= 150 && cm <= 193
                       'n':'i':x ->
                           let cm = read (reverse x)
                           in cm >= 59 && cm <= 76
                       _ -> False) .
                  reverse)) $
            Map.lookup "hgt" m
        hcl =
            maybe
                False
                (\case
                     '#':t ->
                         List.length t == 6 &&
                         all
                             (flip
                                  Set.member
                                  (Set.fromList
                                       [ 'a'
                                       , 'b'
                                       , 'c'
                                       , 'd'
                                       , 'e'
                                       , 'f'
                                       , '0'
                                       , '1'
                                       , '2'
                                       , '3'
                                       , '4'
                                       , '5'
                                       , '6'
                                       , '7'
                                       , '8'
                                       , '9'
                                       ]))
                             t
                     _ -> False) $
            Map.lookup "hcl" m
        ecl =
            maybe
                False
                (flip
                     Set.member
                     (Set.fromList
                          ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])) $
            Map.lookup "ecl" m
        pid =
            maybe
                False
                (\x ->
                     List.length x == 9 &&
                     all
                         (flip
                              Set.member
                              (Set.fromList
                                   [ '0'
                                   , '1'
                                   , '2'
                                   , '3'
                                   , '4'
                                   , '5'
                                   , '6'
                                   , '7'
                                   , '8'
                                   , '9'
                                   ]))
                         x) $
            Map.lookup "pid" m
    in hasAll && byr && iyr && eyr && hgt && hcl && ecl && pid

answer2 :: _ -> _
answer2 = List.length . filter validate2

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
