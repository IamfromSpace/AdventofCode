{-# LANGUAGE Arrows #-}

module Lib where

import AdventOfCode.ArrowParser (AP, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow (arr, returnA, (&&&), (***), (<+>), (<<<), (>>>), (>>^), (|||))
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Foldable (fold, toList)
import Data.Group (invert, (~~))
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum (..))
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Test.Hspec (describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (foldr, init, lookup, map, (++))

toBits :: Char -> [Bool]
toBits '0' = [False, False, False, False]
toBits '1' = [False, False, False, True]
toBits '2' = [False, False, True, False]
toBits '3' = [False, False, True, True]
toBits '4' = [False, True, False, False]
toBits '5' = [False, True, False, True]
toBits '6' = [False, True, True, False]
toBits '7' = [False, True, True, True]
toBits '8' = [True, False, False, False]
toBits '9' = [True, False, False, True]
toBits 'A' = [True, False, True, False]
toBits 'B' = [True, False, True, True]
toBits 'C' = [True, True, False, False]
toBits 'D' = [True, True, False, True]
toBits 'E' = [True, True, True, False]
toBits 'F' = [True, True, True, True]
toBits _ = error "badBit"

parse1 :: String -> _
parse1 = foldMap toBits

parse2 :: String -> _
parse2 = parse1

toNum :: [Bool] -> Int
toNum =
  let go (h : t) = (if h then 1 else 0) + 2 * go t
      go [] = 0
   in go . reverse

toOp :: Int -> Op
toOp 0 = Add
toOp 1 = Mult
toOp 2 = Min
toOp 3 = Max
toOp 5 = GreaterThan
toOp 6 = LessThan
toOp 7 = Equal
toOp _ = error "bad"

data Op
  = Add
  | Mult
  | Min
  | Max
  | GreaterThan
  | LessThan
  | Equal
  deriving (Show, Eq)

data Packet
  = Type4 Int Int
  | Operator
      Int -- Version
      Op
      [Packet]
  deriving (Show, Eq)

loop4 :: AP Bool (Int, [Bool]) (Int, Int)
loop4 = proc (i, prev) -> do
  isMore <- AP.anyToken -< ()
  bits <- AP.count AP.anyToken -< (4, ())
  let n = (i + 1, prev <> bits)
  if isMore
    then loop4 -< n
    else returnA -< fmap toNum n

type4 :: AP Bool () (Int, Packet)
type4 = proc () -> do
  versionBits <- AP.count AP.anyToken -< (3, ())
  AP.foldable [True, False, False] -< ()
  (count, literal) <- loop4 -< (0, [])
  returnA -< (3 + 3 + count * 5, Type4 (toNum versionBits) literal)

testOut4 = AP.parseImpure (type4 >>! AP.end) $ fmap ((==) '1') ("110100101111111000101000" :: String)

bits11 :: AP Bool () [Bool]
bits11 = proc () -> do
  AP.count AP.anyToken -< (11, ())

bits15 :: AP Bool () [Bool]
bits15 = proc () -> do
  AP.count AP.anyToken -< (15, ())

eatPackets :: AP Bool (Int, [Packet]) [Packet]
eatPackets = proc (target, prev) -> do
  (count, p) <- packet -< ()
  let rem = target - count
  let ps = prev <> [p]
  if rem < 0
    then returnA -< error "too many bits consumed!"
    else
      if rem == 0
        then returnA -< ps
        else eatPackets -< (rem, ps)

operator :: AP Bool () (Int, Packet)
operator = proc () -> do
  versionBits <- AP.count AP.anyToken -< (3, ())
  typeBits <- AP.count AP.anyToken -< (3, ())
  isNumberOf <- AP.anyToken -< ()
  (packetLengths, packets) <-
    if isNumberOf
      then do
        lengthBits <- AP.count AP.anyToken -< (11, ())
        pcs <- AP.count packet -< (toNum lengthBits, ())
        returnA -< (foldl (\(bcs, ps) (bc, p) -> (bcs + bc, ps <> [p])) (11, [])) pcs
      else do
        lengthBits <- AP.count AP.anyToken -< (15, ())
        let bitCount = toNum lengthBits
        ps <- eatPackets -< (bitCount, [])
        returnA -< (15 + bitCount, ps) -- need to consume the right length
  returnA -< (3 + 3 + 1 + packetLengths, Operator (toNum versionBits) (toOp (toNum typeBits)) packets)

testOutOp1 = AP.parseImpure packet $ fmap ((==) '1') ("00111000000000000110111101000101001010010001001000000000" :: String)

testOutOp2 = AP.parseImpure packet $ fmap ((==) '1') ("11101110000000001101010000001100100000100011000001100000" :: String)

packet :: AP Bool () (Int, Packet)
packet = type4 <+> operator

sumVersions :: Packet -> Int
sumVersions (Type4 v _) = v
sumVersions (Operator v _ ps) = v + sum (fmap sumVersions ps)

answer1 :: _ -> Int
answer1 = sum . fmap (sumVersions . snd) . AP.parseImpure (AP.many packet)

runCalc :: Packet -> Int
runCalc (Type4 _ lit) = lit
runCalc (Operator _ Add ps) = sum (fmap runCalc ps)
runCalc (Operator _ Mult ps) = product (fmap runCalc ps)
runCalc (Operator _ Min ps) = minimum (fmap runCalc ps)
runCalc (Operator _ Max ps) = maximum (fmap runCalc ps)
runCalc (Operator _ GreaterThan ps) =
  let [a, b] = (fmap runCalc ps)
   in if a > b then 1 else 0
runCalc (Operator _ LessThan ps) =
  let [a, b] = (fmap runCalc ps)
   in if a < b then 1 else 0
runCalc (Operator _ Equal ps) =
  let [a, b] = (fmap runCalc ps)
   in if a == b then 1 else 0

answer2 :: _ -> _
answer2 = head . fmap (runCalc . snd) . AP.parseImpure (AP.many packet)

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ (snd (AP.parseImpure packet (parse1 ("8A004A801A8002F478" :: String)))) `shouldBe` (Operator 4 Add [Operator 1 Add [Operator 5 Add [Type4 6 15]]])
    it "should work" $ (snd (AP.parseImpure packet (parse1 ("620080001611562C8802118E34" :: String)))) `shouldBe` (Operator 0 Add [Operator 0 Add [], Operator 0 Add []])
    it "should work" $ (snd (AP.parseImpure packet (parse1 ("C0015000016115A2E0802F182340" :: String)))) `shouldBe` (Operator 0 Add [])
    it "should work" $ (snd (AP.parseImpure packet (parse1 ("A0016C880162017C3686B18A3D4780" :: String)))) `shouldBe` Operator 0 Add []
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
