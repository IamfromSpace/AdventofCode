{-# LANGUAGE Arrows #-}

module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (Vector (..), multiLines)
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

type Range3 = ((Integer, Integer), (Integer, Integer), (Integer, Integer))

negDec :: APC () Integer
negDec = proc () -> do
  n <- AP.optional (AP.token '-') -< ()
  d <- AP.decimal -< ()
  returnA -< ((if Maybe.isJust n then -1 else 1) * d)

range :: APC () (Integer, Integer)
range = (negDec >>! AP.string "..") &&& negDec

onOff :: APC () Bool
onOff = (AP.string "on" >>^ const True) <+> (AP.string "off" >>^ const False)

line :: APC () (Bool, Range3)
line = proc () -> do
  isOn <- onOff -< ()
  AP.string " x=" -< ()
  x <- range -< ()
  AP.string ",y=" -< ()
  y <- range -< ()
  AP.string ",z=" -< ()
  z <- range -< ()
  returnA -< (isOn, (x, y, z))

order :: (Integer, Integer) -> (Integer, Integer)
order (a, b) = (min a b, max a b)

boundedRange :: (Integer, Integer) -> [Integer]
boundedRange (a, b) = [max a (-50) .. min b 50]

runCube :: ((Integer, Integer) -> [Integer]) -> (Bool, Range3) -> Set (Integer, Integer, Integer) -> Set (Integer, Integer, Integer)
runCube r (isOn, (x, y, z)) init =
  let ps = App.liftA3 (,,) (r (order x)) (r (order y)) (r (order z))
      op = if isOn then Set.insert else Set.delete
   in foldl (flip op) init ps

runAllCubes :: _ -> [(Bool, Range3)] -> Set (Integer, Integer, Integer)
runAllCubes r = foldl (flip (runCube r)) mempty

parse1 :: String -> _
parse1 = AP.parseImpure (AP.linesOf line)

parse2 :: String -> _
parse2 = parse1

answer1 :: _ -> _
answer1 = Set.size . runAllCubes boundedRange

data RangeResult
  = LeftInRight
  | NoIntersection
  | Intesection Range3 Range3

compOrSplit :: Range3 -> Range3 -> RangeResult
compOrSplit ((minX1, maxX1), (minY1, maxY1), (minZ1, maxZ1)) ((minX2, maxX2), (minY2, maxY2), (minZ2, maxZ2)) =
  if maxX1 < minX2
    || minX1 > maxX2
    || maxY1 < minY2
    || minY1 > maxY2
    || maxZ1 < minZ2
    || minZ1 > maxZ2
    then NoIntersection
    else
      if minX1 >= minX2
        && maxX1 <= maxX2
        && minY1 >= minY2
        && maxY1 <= maxY2
        && minZ1 >= minZ2
        && maxZ1 <= maxZ2
        then LeftInRight
        else
          if not (maxX1 < minX2 || minX1 > maxX2) && not (minX1 >= minX2 && maxX1 <= maxX2)
            then
              let (newLength, check) = (maxX1 - minX1 + 1) `divMod` 2
               in if check == 1
                    then error "underflow x"
                    else Intesection ((minX1, maxX1 - newLength), (minY1, maxY1), (minZ1, maxZ1)) ((minX1 + newLength, maxX1), (minY1, maxY1), (minZ1, maxZ1))
            else
              if not (maxY1 < minY2 || minY1 > maxY2) && not (minY1 >= minY2 && maxY1 <= maxY2)
                then
                  let (newLength, check) = (maxY1 - minY1 + 1) `divMod` 2
                   in if check == 1
                        then error "underflow y"
                        else Intesection ((minX1, maxX1), (minY1, maxY1 - newLength), (minZ1, maxZ1)) ((minX1, maxX1), (minY1 + newLength, maxY1), (minZ1, maxZ1))
                else
                  if not (maxZ1 < minZ2 || minZ1 > maxZ2) && not (minZ1 >= minZ2 && maxZ1 <= maxZ2)
                    then
                      let (newLength, check) = (maxZ1 - minZ1 + 1) `divMod` 2
                       in if check == 1
                            then error "underflow z"
                            else Intesection ((minX1, maxX1), (minY1, maxY1), (minZ1, maxZ1 - newLength)) ((minX1, maxX1), (minY1, maxY1), (minZ1 + newLength, maxZ1))
                    else NoIntersection

countPoints :: Range3 -> Integer
countPoints ((minX, maxX), (minY, maxY), (minZ, maxZ)) =
  (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)

count ::
  Range3 ->
  [(Bool, Range3)] -> -- Make sure to reverse this list, because the later ones get priority
  Integer
count _ [] = 0
count exploring xs@((isOn, h) : t) =
  case compOrSplit exploring h of
    LeftInRight -> if isOn then countPoints exploring else 0
    NoIntersection -> count exploring t
    Intesection a b -> count a xs + count b xs

answer2 :: _ -> _
answer2 xs =
  let ready = fmap (fmap (\(x, y, z) -> (order x, order y, order z))) $ reverse xs
      mag = 17 -- none of the big cubes live within +/-2^15 of the center
      d = (-2 ^ mag, 2 ^ mag - 1)
   in count (d, d, d) ready

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

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
