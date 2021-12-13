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

foldParser = ((AP.string "fold along " !>> (AP.anyToken >>! AP.token '=')) &&& AP.decimal)

type V2 = Util.Vector (Integer, Integer)

parse1 :: String -> _
parse1 s =
  let [a, b] = Split.splitOn "\n\n" s
   in (Set.fromList $ fmap Util.Vector $ AP.parseImpure (AP.linesOf ((AP.decimal >>! AP.token ',') &&& AP.decimal)) a, AP.parseImpure (AP.linesOf foldParser) b)

foldPaper :: (Char, Integer) -> Set V2 -> Set V2
foldPaper ('y', line) s =
  let leftSide = filter (\(Util.Vector (x, y)) -> y < line) $ Set.toList s
      rightSide = fmap (\(Util.Vector (x, y)) -> Util.Vector (x, (-1) * (y - line * 2))) $ filter (\(Util.Vector (x, y)) -> y > line) $ Set.toList s
   in Set.fromList (leftSide <> rightSide)
foldPaper ('x', line) s =
  let leftSide = filter (\(Util.Vector (x, y)) -> x < line) $ Set.toList s
      rightSide = fmap (\(Util.Vector (x, y)) -> Util.Vector ((-1) * (x - line * 2), y)) $ filter (\(Util.Vector (x, y)) -> x > line) $ Set.toList s
   in Set.fromList (leftSide <> rightSide)

parse2 :: String -> _
parse2 = parse1

answer1 :: _ -> _
answer1 (p, insts) = Set.size $ foldPaper (head insts) p

answer2 :: _ -> _
answer2 (p, insts) = Util.prettyPrintPointSetFlippable True '-' '#' $ Set.map (\(Util.Vector v) -> v) $ foldl (\s inst -> foldPaper inst s) p insts

show1 :: Show _a => _a -> String
show1 = show

show2 :: _ -> String
show2 = id

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ 'a' `shouldBe` 'a'
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
