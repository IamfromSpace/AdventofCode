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
import Data.Map.Monoidal (MonoidalMap (..))
import qualified Data.Map.Monoidal as MMap
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

parse1 :: String -> _
parse1 _ = (7, 9)

parse2 :: String -> _
parse2 = parse1

sMod x y = ((x - 1) `mod` y) + 1

playFirst :: (Bool, (Int, (Int, Int), (Int, Int))) -> (Bool, (Int, (Int, Int), (Int, Int)))
playFirst x@(True, _) = x
playFirst (_, (i, (f, s), (pf, ps))) =
  let pf' = ((i `sMod` 100) + ((i + 1) `sMod` 100) + ((i + 2) `sMod` 100) + pf) `sMod` 10
      score = f + pf'
   in (score >= 1000, (i + 3, (score, s), (pf', ps)))

playSecond :: (Bool, (Int, (Int, Int), (Int, Int))) -> (Bool, (Int, (Int, Int), (Int, Int)))
playSecond x@(True, _) = x
playSecond (_, (i, (f, s), (pf, ps))) =
  let ps' = ((i `sMod` 100) + ((i + 1) `sMod` 100) + ((i + 2) `sMod` 100) + ps) `sMod` 10
      score = s + ps'
   in (score >= 1000, (i + 3, (f, score), (pf, ps')))

play :: (Bool, (Int, (Int, Int), (Int, Int))) -> (Bool, (Int, (Int, Int), (Int, Int)))
play = playSecond . playFirst

answer1 :: _ -> _
answer1 s =
  let (_, (rolls, (as, bs), _)) = Util.labelTrace "head" $ head $ dropWhile (not . fst) $ iterate play (False, (1, (0, 0), s))
   in (rolls - 1) * min as bs

qDie = [1, 2, 3]

playFirst2 :: (Bool, (Int, Int), (Int, Int)) -> [(Bool, (Int, Int), (Int, Int))]
playFirst2 x@(True, _, _) = [x]
playFirst2 (_, (f, s), (pf, ps)) = do
  r1 <- qDie
  r2 <- qDie
  r3 <- qDie
  let pf' = (pf + r1 + r2 + r3) `sMod` 10
  let score = f + pf'
  return (score >= 21, (score, s), (pf', ps))

playSecond2 :: (Bool, (Int, Int), (Int, Int)) -> [(Bool, (Int, Int), (Int, Int))]
playSecond2 x@(True, _, _) = [x]
playSecond2 (_, (f, s), (pf, ps)) = do
  r1 <- qDie
  r2 <- qDie
  r3 <- qDie
  let ps' = (ps + r1 + r2 + r3) `sMod` 10
  let score = s + ps'
  return (score >= 21, (f, score), (pf, ps'))

play2 :: (Bool, (Int, Int), (Int, Int)) -> [(Bool, (Int, Int), (Int, Int))]
play2 x = do
  afterFirst <- playFirst2 x
  playSecond2 afterFirst

playOut :: (Bool, (Int, Int), (Int, Int)) -> [(Bool, (Int, Int), (Int, Int))]
playOut x = do
  afterRound@(done, _, _) <- play2 x
  if done
    then return afterRound
    else playOut afterRound

playOutCounts :: (Int, Int) -> (Sum Int, Sum Int)
playOutCounts s = fold $ do
  (_, (f, s), _) <- playOut (False, (0, 0), s)
  return $
    if f > s
      then (Sum 1, Sum 0)
      else (Sum 0, Sum 1)

qSum :: [Int]
qSum = App.liftA3 (\a b c -> a + b + c) qDie qDie qDie

type State =
  Either
    Bool -- First won
    ( Bool, -- It's First's turn
      (Int, Int), -- Scores
      (Int, Int) -- Positions
    )

qSumFreq :: MonoidalMap Int (Sum Int)
qSumFreq =
  foldMap (\x -> MMap.singleton x (Sum 1)) qSum

toNexts' :: Int -> MonoidalMap Int (Sum Int)
toNexts' p =
  foldMap (\(off, c) -> MMap.singleton ((p + off) `sMod` 10) c) $ MMap.toList qSumFreq

mmap f = MonoidalMap . Map.map f . getMonoidalMap

toNexts :: MonoidalMap Int (Sum Int) -> MonoidalMap Int (Sum Int)
toNexts positionFreqs =
  foldMap
    (\(p, count) -> mmap ((*) count) $ toNexts' p)
    $ MMap.toList positionFreqs

play3 :: State -> MonoidalMap State (Sum Int)
play3 x@(Left _) = MMap.singleton x 1
play3 (Right (True, (f, s), (pf, ps))) =
  foldMap
    ( \(pf', c) ->
        let score = f + pf'
         in if score >= 21
              then MMap.singleton (Left True) c
              else MMap.singleton (Right (False, (score, s), (pf', ps))) c
    )
    $ MMap.toList $ toNexts' pf
play3 (Right (False, (f, s), (pf, ps))) =
  foldMap
    ( \(ps', c) ->
        let score = s + ps'
         in if score >= 21
              then MMap.singleton (Left False) c
              else MMap.singleton (Right (True, (f, score), (pf, ps'))) c
    )
    $ MMap.toList $ toNexts' ps

play3Rep :: MonoidalMap State (Sum Int) -> MonoidalMap State (Sum Int)
play3Rep =
  foldMap
    (\(p, count) -> mmap ((*) count) $ play3 p)
    . MMap.toList

answer2 :: _ -> _
answer2 s = Util.applyNTimes play3Rep (MMap.singleton (Right (True, (0, 0), s)) 1) 19

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ answer1 (4, 8) `shouldBe` 739785
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
