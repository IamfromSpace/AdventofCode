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

parse1 :: String -> [Int]
parse1 s = read ("[" <> s <> "]") :: [Int]

parse2 :: String -> _
parse2 = parse1

-- So ugly, lol
toCounts :: [Int] -> (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int)
toCounts [] t = t
toCounts (0 : t) (x0, x1, x2, x3, x4, x5, x6, x7, x8) = toCounts t (x0 + 1, x1, x2, x3, x4, x5, x6, x7, x8)
toCounts (1 : t) (x0, x1, x2, x3, x4, x5, x6, x7, x8) = toCounts t (x0, x1 + 1, x2, x3, x4, x5, x6, x7, x8)
toCounts (2 : t) (x0, x1, x2, x3, x4, x5, x6, x7, x8) = toCounts t (x0, x1, x2 + 1, x3, x4, x5, x6, x7, x8)
toCounts (3 : t) (x0, x1, x2, x3, x4, x5, x6, x7, x8) = toCounts t (x0, x1, x2, x3 + 1, x4, x5, x6, x7, x8)
toCounts (4 : t) (x0, x1, x2, x3, x4, x5, x6, x7, x8) = toCounts t (x0, x1, x2, x3, x4 + 1, x5, x6, x7, x8)
toCounts (5 : t) (x0, x1, x2, x3, x4, x5, x6, x7, x8) = toCounts t (x0, x1, x2, x3, x4, x5 + 1, x6, x7, x8)
toCounts (6 : t) (x0, x1, x2, x3, x4, x5, x6, x7, x8) = toCounts t (x0, x1, x2, x3, x4, x5, x6 + 1, x7, x8)
toCounts _ _ = error "bad"

day :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int)
day (x0, x1, x2, x3, x4, x5, x6, x7, x8) = (x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0)

addUp :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int
addUp (x0, x1, x2, x3, x4, x5, x6, x7, x8) = x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8

answer1 :: _ -> _
answer1 is = addUp $ Util.applyNTimes day (toCounts is (0, 0, 0, 0, 0, 0, 0, 0, 0)) 80

answer2 :: _ -> _
answer2 is = addUp $ Util.applyNTimes day (toCounts is (0, 0, 0, 0, 0, 0, 0, 0, 0)) 256

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
