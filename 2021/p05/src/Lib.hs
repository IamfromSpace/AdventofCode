module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
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

p :: APC () (Integer, Integer)
p = (AP.decimal >>! AP.token ',') &&& AP.decimal

line :: APC () ((Integer, Integer), (Integer, Integer))
line = (p >>! AP.string " -> ") &&& p

parse1 :: String -> _
parse1 = AP.parseImpure (AP.linesOf line)

parse2 :: String -> _
parse2 = parse1

isHorOrVer :: ((Integer, Integer), (Integer, Integer)) -> Bool
isHorOrVer ((x1, y1), (x2, y2)) =
  x1 == x2 || y1 == y2

linePoints :: ((Integer, Integer), (Integer, Integer)) -> [(Integer, Integer)]
linePoints ((x1, y1), (x2, y2)) =
  if x1 == x2
    then fmap (\y -> (x1, y)) [min y1 y2 .. max y1 y2]
    else
      if y1 == y2
        then fmap (\x -> (x, y1)) [min x1 x2 .. max x1 x2]
        else [] -- Diagonal

overlaps :: _ -> [((Integer, Integer), (Integer, Integer))] -> (Set (Integer, Integer), Set (Integer, Integer)) -> Set (Integer, Integer)
overlaps f [] (_, os) = os
overlaps f (h : t) s =
  overlaps f t $
    List.foldr
      ( \p (seen, os) ->
          if Set.member p seen
            then (seen, Set.insert p os)
            else (Set.insert p seen, os)
      )
      s
      $ f h

answer1 :: _ -> _
answer1 s = Set.size $ overlaps linePoints s mempty

linePoints2 :: ((Integer, Integer), (Integer, Integer)) -> [(Integer, Integer)]
linePoints2 ((x1, y1), (x2, y2)) =
  if x1 == x2
    then fmap (\y -> (x1, y)) [min y1 y2 .. max y1 y2]
    else
      if y1 == y2
        then fmap (\x -> (x, y1)) [min x1 x2 .. max x1 x2]
        else zip (if x2 > x1 then [x1 .. x2] else reverse [x2 .. x1]) (if y2 > y1 then [y1 .. y2] else reverse [y2 .. y1])

answer2 :: _ -> _
answer2 s = Set.size $ overlaps linePoints2 s mempty

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

tests :: _
tests = do
  describe "pure components" $ do
    it "should parse" $ parse1 undefined `shouldBe` undefined
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
