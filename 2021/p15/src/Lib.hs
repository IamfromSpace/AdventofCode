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

type V2 = Util.Vector (Integer, Integer)

parse1 :: String -> Map V2 (Sum Int)
parse1 = Map.map Sum . Util.parseGrid (\v c m -> Map.insert (Util.Vector v) (read [c]) m) mempty

parse2 :: String -> _
parse2 = parse1

ns = fmap Util.Vector [(-1, 0), (1, 0), (0, -1), (0, 1)]

options :: Map V2 (Sum Int) -> V2 -> [Util.AStarStepOption2 _ _]
options m p =
  Maybe.mapMaybe (\o -> fmap (Util.AStarStepOption2 o) $ Map.lookup o m) $ fmap ((<>) p) ns

heuristic :: V2 -> V2 -> Sum Int
heuristic t p = Sum $ fromIntegral $ Util.manLen (p ~~ t)

answer :: _ -> _
answer t m = getSum $ fst $ Maybe.fromJust $ Util.aStar2 (heuristic t) (options m) mempty

answer1 :: _ -> _
answer1 = answer (Util.Vector (99, 99))

options2 :: Map V2 (Sum Int) -> V2 -> [Util.AStarStepOption2 _ _]
options2 m p =
  Maybe.mapMaybe
    ( \o@(Util.Vector (x, y)) ->
        let (xOff, x') = x `divMod` 100
            (yOff, y') = y `divMod` 100
         in if xOff >= 0 && xOff < 5 && yOff >= 0 && yOff < 5
              then
                fmap
                  (\(Sum r) -> Util.AStarStepOption2 o (Sum ((((fromIntegral r) + yOff + xOff - 1) `mod` 9) + 1)))
                  $ Map.lookup (Util.Vector (x', y')) m
              else Nothing
    )
    $ fmap ((<>) p) ns

heuristic2 :: V2 -> V2 -> Sum Integer
heuristic2 t p = Sum $ Util.manLen (p ~~ t)

answer2 :: _ -> _
answer2 m = getSum $ fst $ Maybe.fromJust $ Util.aStar2 (heuristic2 (Util.Vector (499, 499))) (options2 m) mempty

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ 'a' `shouldBe` 'a'
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer (Util.Vector (9, 9)) . parse1)
    it "example 1" $ p1 "./ex1.txt" 40
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
