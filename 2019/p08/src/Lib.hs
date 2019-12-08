module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util ()
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split (chunksOf)
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

parse1 :: String -> _
parse1 = id

parse2 :: String -> _
parse2 = parse1

toLayers :: Int -> Int -> String -> [String]
toLayers h w s = chunksOf (h * w) s

toLines :: Int -> String -> [String]
toLines = chunksOf

answer1 :: _ -> _
answer1 y =
    let layer =
            snd $
            minimum $
            fmap (\x -> (length $ filter ((==) '0') x, x)) $ toLayers 25 6 y
        ones = length $ filter ((==) '1') layer
        twos = length $ filter ((==) '2') layer
    in ones * twos

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN _ ([]:_) = []
zipN fn xs = fn (fmap head xs) : zipN fn (fmap tail xs)

layer :: String -> Char
layer s = go '2' s
  where
    go c [] =
        if c == '2'
            then '0'
            else c
    go '0' _ = '0'
    go '1' _ = '1'
    go '2' (h:t) = go h t

answer2 :: _ -> _
answer2 x = unlines $ chunksOf 25 $ zipN layer $ chunksOf (25 * 6) x
