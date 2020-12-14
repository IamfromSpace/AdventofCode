module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       ((&&&), (***), (<+>), (<<<), (>>>), (|||), arr)
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import Data.Bits ((.&.), (.|.))
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

data Inst
    = Mask String
    | Mem Int
          Int
    deriving (Show)

parseMem :: String -> Int
parseMem s =
    let ["mem", x, _] = Split.splitOneOf "[]" s
    in read x

parseLine :: String -> Inst
parseLine s =
    case Split.splitOn " = " s of
        ["mask", x] -> Mask x
        [mem, x] -> Mem (parseMem mem) (read x)

parse1 :: String -> _
parse1 = fmap parseLine . lines

parse2 :: String -> _
parse2 = parse1

maskToBits :: String -> (Int, Int)
maskToBits m =
    let ors =
            List.foldl' (\acc next -> acc * 2 + next) 0 $
            fmap
                (\x ->
                     if x == '1'
                         then 1
                         else 0)
                m
        ands =
            List.foldl' (\acc next -> acc * 2 + next) 0 $
            fmap
                (\x ->
                     if x == '0'
                         then 0
                         else 1)
                m
    in (ors, ands)

step :: Inst -> (String, Map Int Int) -> (String, Map Int Int)
step inst (mask, mem) =
    case inst of
        Mask x -> (x, mem)
        Mem to val ->
            let (ors, ands) = maskToBits mask
            in (mask, Map.insert to ((val .|. ors) .&. ands) mem)

answer1 :: [_] -> _
answer1 = sum . Map.elems . snd . List.foldl' (flip step) ("", mempty)

show36Bit' :: Int -> Int -> String
show36Bit' 36 x = []
show36Bit' n x =
    let (d, m) = x `divMod` 2
    in (if m == 1
            then '1'
            else '0') :
       show36Bit' (n + 1) d

show36Bit :: Int -> String
show36Bit = reverse . show36Bit' 0

strMask :: String -> Int -> String
strMask mask i =
    List.zipWith
        (\mC iC ->
             case mC of
                 '0' -> iC
                 _ -> mC)
        mask
        (show36Bit i)

maskToBits2 :: String -> [Int]
maskToBits2 =
    fmap (List.foldl' (\acc next -> acc * 2 + next) 0) .
    sequence .
    fmap
        (\case
             '1' -> [1]
             '0' -> [0]
             'X' -> [0, 1])

mask2 :: String -> Int -> [Int]
mask2 mask = maskToBits2 . strMask mask

step2 :: Inst -> (String, Map Int Int) -> (String, Map Int Int)
step2 inst (mask, mem) =
    case inst of
        Mask x -> (x, mem)
        Mem to val ->
            (mask, List.foldl' (\m t -> Map.insert t val m) mem $ mask2 mask to)

answer2 :: [Inst] -> Int
answer2 = sum . Map.elems . snd . List.foldl' (flip step2) ("", mempty)

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
