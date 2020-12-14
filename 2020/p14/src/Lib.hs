module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import Control.Arrow
       (Arrow, (&&&), (***), (<+>), (<<<), (>>>), (>>^), (|||), arr)
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

(!>>) :: Arrow a => a b ignore -> a b c -> a b c
(!>>) a b = (a &&& b) >>^ snd

int :: MD String Int
int = PA.many PA.digit >>^ read

mask :: MD String Inst
mask = (PA.string "mask = " !>> PA.many (PA.anyOf "01X")) >>^ Mask

mem :: MD String Inst
mem =
    (((PA.string "mem[") !>> int) &&& ((PA.string "] = ") !>> int)) >>^
    (uncurry Mem)

line :: MD String Inst
line = mask <+> mem

input :: MD String [Inst]
input = PA.sepBy1 line (PA.char '\n')

parse1 :: String -> [Inst]
parse1 = either (error . unlines) id . PA.runParser input

parse2 :: String -> _
parse2 = parse1

mask1ToMaskFn :: String -> Int -> Int
mask1ToMaskFn m =
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
    in ((.&.) ands) . ((.|.) ors)

step :: Inst -> (Int -> Int, Map Int Int) -> (Int -> Int, Map Int Int)
step inst (maskFn, mem) =
    case inst of
        Mask x -> (mask1ToMaskFn x, mem)
        Mem to val -> (maskFn, Map.insert to (maskFn val) mem)

answer1 :: [_] -> _
answer1 = sum . Map.elems . snd . List.foldl' (flip step) (id, mempty)

mask2ToMask1s :: String -> [String]
mask2ToMask1s =
    sequence .
    fmap
        (\case
             '1' -> ['1']
             '0' -> ['X']
             'X' -> ['0', '1'])

mask2ToMaskFns :: String -> [Int -> Int]
mask2ToMaskFns = fmap (mask1ToMaskFn) . mask2ToMask1s

step2 :: Inst -> ([Int -> Int], Map Int Int) -> ([Int -> Int], Map Int Int)
step2 inst (maskFns, mem) =
    case inst of
        Mask x -> (mask2ToMaskFns x, mem)
        Mem to val ->
            ( maskFns
            , List.foldl' (\m t -> Map.insert t val m) mem $
              fmap (\f -> f to) maskFns)

answer2 :: [Inst] -> Int
answer2 = sum . Map.elems . snd . List.foldl' (flip step2) ([], mempty)

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
