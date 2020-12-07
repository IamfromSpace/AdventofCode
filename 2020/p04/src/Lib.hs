module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow ((>>>))
import Control.Monad ((>=>), guard)
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

parseLine :: String -> [(String, String)]
parseLine s =
    let ts = Split.splitOn " " (s)
    in fmap ((\[a, b] -> (a, b)) . Split.splitOn ":") ts

parseLines :: [String] -> Map String String
parseLines = Map.fromList . foldMap parseLine

parse1 :: String -> [Map String String]
parse1 = fmap parseLines . multiLines

parse2 :: String -> _
parse2 = parse1

validate :: Map String String -> Bool
validate m =
    all
        (flip Set.member (Set.fromList $ Map.keys m))
        ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- no cid?

answer1 :: _ -> _
answer1 = List.length . filter validate

data Dim
    = Cm Int
    | In Int

vByr :: Int -> Bool
vByr x = x >= 1920 && x <= 2002

vIyr :: Int -> Bool
vIyr x = x >= 2010 && x <= 2020

vEyr :: Int -> Bool
vEyr x = x >= 2020 && x <= 2030

pHgt :: String -> Maybe Dim
pHgt s =
    case reverse s of
        'm':'c':x -> Cm <$> readMaybe (reverse x)
        'n':'i':x -> In <$> readMaybe (reverse x)
        _ -> Nothing

vHgt :: Dim -> Bool
vHgt (Cm x) = x >= 150 && x <= 193
vHgt (In x) = x >= 59 && x <= 76

isLowerHex :: Char -> Bool
isLowerHex x = Char.isHexDigit x && Char.isLower x || Char.isDigit x

pHcl :: String -> Maybe String
pHcl x =
    case x of
        '#':t -> Just t
        _ -> Nothing

vHcl :: String -> Bool
vHcl x = List.length x == 6 && (all (isLowerHex) x)

vEcl :: String -> Bool
vEcl =
    flip
        Set.member
        (Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

vPid :: String -> Bool
vPid x = List.length x == 9 && all Char.isDigit x

validations :: [Map String String -> Bool]
validations =
    let f key parse val =
            (Map.lookup key >=> parse >=> (pure . val)) >>>
            Maybe.fromMaybe False
    in [ f "byr" readMaybe vByr
       , f "iyr" readMaybe vIyr
       , f "eyr" readMaybe vEyr
       , f "hgt" pHgt vHgt
       , f "hcl" pHcl vHcl
       , f "ecl" pure vEcl
       , f "pid" pure vEcl
       ]

validate2 :: Map String String -> Bool
validate2 = and . sequence validations

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
