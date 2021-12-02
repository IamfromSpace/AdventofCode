module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow
  ( arr,
    returnA,
    (&&&),
    (***),
    (<+>),
    (<<<),
    (>>>),
    (>>^),
    (|||),
  )
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
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
import Data.Monoid (Sum (..))
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Test.Hspec (describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (init, lookup, map, (++))

type V2 = Util.Vector (Integer, Integer)

forward =
  (AP.string "forward " !>> AP.digit) >>^ (\x -> Util.Vector (read [x], 0))

down = (AP.string "down " !>> AP.digit) >>^ (\y -> Util.Vector (0, read [y]))

up = (AP.string "up " !>> AP.digit) >>^ (\y -> Util.Vector (0, -1 * read [y]))

parse1 :: String -> [V2]
parse1 = AP.parseImpure (AP.linesOf (forward <+> down <+> up))

parse2 :: String -> _
parse2 = parse1

answer1 :: [V2] -> _
answer1 = (\(Util.Vector (x, y)) -> x * y) . foldr (<>) mempty

run :: (Integer, V2) -> V2 -> (Integer, V2)
run (aim, cur) next =
  case next of
    Util.Vector (0, y) -> (aim + y, cur)
    Util.Vector (x, _) -> (aim, cur <> (Util.Vector (x, aim * x)))

answer2 :: _ -> _
answer2 = (\(Util.Vector (x, y)) -> x * y) . snd . foldl run (0, mempty)

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

tests :: _
tests = do
  describe "pure components" $ do
    it "should parse" $ answer1 (parse1 ex) `shouldBe` 150
  describe "pure components" $ do
    it "should parse" $ answer2 (parse1 ex) `shouldBe` 900
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 5" $ p2 "./ex2_5.txt" undefined
