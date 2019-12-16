module Lib
    ( parse1
    , parse2
    , answer1
    , answer2
    , phaseStep
    , phase
    , keepOnes
    ) where

--import Prelude hiding (lookup)
--import Data.ByteString (ByteString)
--import Data.Map (Map)
--import Data.Set (Set)
import AdventOfCode.Util (applyNTimes, elmTrace)
import Control.Applicative ()
import Control.Monad.State.Lazy ()
import Crypto.Hash.MD5 ()
import qualified Data.ByteString as BS ()
import Data.ByteString.UTF8 ()
import Data.List ()
import Data.List.Split ()
import qualified Data.Map as Map ()
import Data.Maybe ()
import Data.Monoid ()
import Data.Sequence ()
import qualified Data.Set as Set ()

parse1 :: String -> [Integer]
parse1 = fmap (read . pure)

parse2 :: String -> _
parse2 = parse1

keepOnes :: Integer -> Integer
keepOnes x = abs x `mod` 10

base = [1, 0, -1, 0]

forIndex :: Integer -> [Integer]
forIndex (fromIntegral -> i) = concatMap (replicate (i + 1)) base

phaseStep :: Integer -> Integer -> [Integer] -> Integer
phaseStep start i list =
    keepOnes $
    sum $
    zipWith
        (*)
        (drop (fromIntegral (i - start)) list)
        (take (length list) (cycle (forIndex i)))

phase :: Integer -> [Integer] -> [Integer]
phase start list =
    fmap
        (flip (phaseStep start) list)
        [start .. fromIntegral (length list - 1) + start]

answer1 :: _ -> _
answer1 x = fmap (head . show) $ take 8 $ applyNTimes (phase 0) x 100

simple :: [Integer] -> [Integer]
simple [] = []
simple list@(_:t) = keepOnes (sum list) : simple t

simpler' :: Integer -> [Integer] -> [Integer]
simpler' _ [] = []
simpler' !sum !(h:t) = h + sum : simpler' (sum + h) t

simpler :: [Integer] -> [Integer]
simpler = fmap keepOnes . reverse . simpler' 0 . reverse

answer2 :: [Integer] -> _
answer2 input =
    let offset = read $ fmap (head . show) $ take 7 input
        fullInput = take (length input * 10000) $ cycle input
    in fmap (head . show) $
       take 8 $ applyNTimes simpler (drop offset fullInput) 100
