module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , show1
    , show2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
import           Data.Map                 (Map)

--import Data.Set (Set)
import           AdventOfCode.Util        ()
import           Control.Applicative      ()
import           Control.Monad.State.Lazy ()
import           Crypto.Hash.MD5          ()
import qualified Data.ByteString          as BS ()
import           Data.ByteString.UTF8     ()
import           Data.List                (foldl')
import           Data.List.Split          ()
import qualified Data.Map                 as Map (foldrWithKey, singleton,
                                                  unionWith)
import           Data.Maybe               (fromJust)
import           Data.Monoid              ()
import qualified Data.Set                 as Set ()

parse1 :: String -> [String]
parse1 = lines

parse2 :: String -> _
parse2 = parse1

asFreqs :: String -> [Map Char Int]
asFreqs = fmap (\c -> Map.singleton c 1)

addFreqs :: [Map Char Int] -> [Map Char Int] -> [Map Char Int]
addFreqs = zipWith (Map.unionWith (+))

greatest :: Map Char Int -> Maybe Char
greatest =
    fmap fst .
    Map.foldrWithKey
        (\char count mGreatest ->
             case mGreatest of
                 Nothing -> Just (char, count)
                 Just (_, co) ->
                     if count >= co
                         then Just (char, count)
                         else mGreatest)
        Nothing

answer1 :: [String] -> String
answer1 xs =
    let xxs = fmap asFreqs xs
    in fromJust $ traverse greatest $ foldl' addFreqs (head xxs) (tail xxs)

least :: Map Char Int -> Maybe Char
least =
    fmap fst .
    Map.foldrWithKey
        (\char count mGreatest ->
             case mGreatest of
                 Nothing -> Just (char, count)
                 Just (_, co) ->
                     if count <= co
                         then Just (char, count)
                         else mGreatest)
        Nothing

answer2 :: [String] -> String
answer2 xs =
    let xxs = fmap asFreqs xs
    in fromJust $ traverse least $ foldl' addFreqs (head xxs) (tail xxs)

show1 :: String -> String
show1 = id

show2 :: String -> String
show2 = id
