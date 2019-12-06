module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import Data.Map (Map)

import AdventOfCode.Util ()
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split (splitOn)
import qualified Data.Map as Map (fromList, keys, lookup)
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import Data.Set (Set)
import qualified Data.Set as Set (fromList, intersection, toList)

parseLine :: String -> (String, String)
parseLine x =
    let [a, b] = splitOn ")" x
    in (b, a)

parse1 :: String -> Map String String
parse1 = Map.fromList . fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

countOrbits :: String -> Map String String -> String -> Int
countOrbits t m s =
    case Map.lookup s m of
        Just p ->
            if p == t
                then 1
                else 1 + countOrbits t m p

answer1 :: _ -> _
answer1 m = sum $ fmap (countOrbits "COM" m) $ Map.keys m

children' :: Map String String -> String -> [String]
children' m s =
    case Map.lookup s m of
        Just "COM" -> ["COM"]
        Just p -> p : children' m p

children :: Map String String -> String -> Set String
children m s = Set.fromList $ children' m s

answer2 :: _ -> _
answer2 m =
    let sharedChildren = children m "YOU" `Set.intersection` children m "SAN"
    in minimum $
       fmap (\c -> (countOrbits c m "YOU" + countOrbits c m "SAN" - 2, c)) $
       Set.toList sharedChildren
