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

unboundedRange :: (Integer, Integer) -> [Integer]
unboundedRange (a, b) = [a .. b]

type V3 = Vector (Integer, Integer, Integer)

data RangeResult
  = NoContact
  | LeftInRight
  | SomeContact

-- Always inclusive!
comparison :: Range3 -> Range3 -> RangeResult
comparison ((minX1, maxX1), (minY1, maxY1), (minZ1, maxZ1)) ((minX2, maxX2), (minY2, maxY2), (minZ2, maxZ2)) =
  if minX1 >= minX2
    && maxX1 <= maxX2
    && minY1 >= minY2
    && maxY1 <= maxY2
    && minZ1 >= minZ2
    && maxZ1 <= maxZ2
    then LeftInRight
    else
      if maxX1 < minX2
        || minX1 > maxX2
        || maxY1 < minY2
        || minY1 > maxY2
        || maxZ1 < minZ2
        || minZ1 > maxZ2
        then NoContact
        else SomeContact

-- Split a 3 dimensional range into 8 equal pieces, everything must be divisible by 2
binSplit :: Range3 -> [Range3]
binSplit r@((minX, maxX), (minY, maxY), (minZ, maxZ)) =
  let (newLength, check) = (maxX - minX + 1) `divMod` 2
   in if check == 1
        then error ("not div by 2: " <> show r)
        else
          [ ((minX, minX + newLength - 1), (minY, minY + newLength - 1), (minZ, minZ + newLength - 1)),
            ((maxX - newLength + 1, maxX), (minY, minY + newLength - 1), (minZ, minZ + newLength - 1)),
            ((minX, minX + newLength - 1), (maxY - newLength + 1, maxY), (minZ, minZ + newLength - 1)),
            ((minX, minX + newLength - 1), (minY, minY + newLength - 1), (maxZ - newLength + 1, maxZ)),
            ((maxX - newLength + 1, maxX), (maxY - newLength + 1, maxY), (minZ, minZ + newLength - 1)),
            ((maxX - newLength + 1, maxX), (minY, minY + newLength - 1), (maxZ - newLength + 1, maxZ)),
            ((minX, minX + newLength - 1), (maxY - newLength + 1, maxY), (maxZ - newLength + 1, maxZ)),
            ((maxX - newLength + 1, maxX), (maxY - newLength + 1, maxY), (maxZ - newLength + 1, maxZ))
          ]

data CoSResult
  = LIR
  | NC
  | S Range3 Range3

compOrSplit :: Range3 -> Range3 -> CoSResult
compOrSplit ((minX1, maxX1), (minY1, maxY1), (minZ1, maxZ1)) ((minX2, maxX2), (minY2, maxY2), (minZ2, maxZ2)) =
  if maxX1 < minX2
    || minX1 > maxX2
    || maxY1 < minY2
    || minY1 > maxY2
    || maxZ1 < minZ2
    || minZ1 > maxZ2
    then NC
    else
      if minX1 >= minX2
        && maxX1 <= maxX2
        && minY1 >= minY2
        && maxY1 <= maxY2
        && minZ1 >= minZ2
        && maxZ1 <= maxZ2
        then LIR
        else
          if not (maxX1 < minX2 || minX1 > maxX2) && not (minX1 >= minX2 && maxX1 <= maxX2)
            then
              let (newLength, check) = (maxX1 - minX1 + 1) `divMod` 2
               in if check == 1
                    then error "underflow x"
                    else S ((minX1, maxX1 - newLength), (minY1, maxY1), (minZ1, maxZ1)) ((minX1 + newLength, maxX1), (minY1, maxY1), (minZ1, maxZ1))
            else
              if not (maxY1 < minY2 || minY1 > maxY2) && not (minY1 >= minY2 && maxY1 <= maxY2)
                then
                  let (newLength, check) = (maxY1 - minY1 + 1) `divMod` 2
                   in if check == 1
                        then error "underflow y"
                        else S ((minX1, maxX1), (minY1, maxY1 - newLength), (minZ1, maxZ1)) ((minX1, maxX1), (minY1 + newLength, maxY1), (minZ1, maxZ1))
                else
                  if not (maxZ1 < minZ2 || minZ1 > maxZ2) && not (minZ1 >= minZ2 && maxZ1 <= maxZ2)
                    then
                      let (newLength, check) = (maxZ1 - minZ1 + 1) `divMod` 2
                       in if check == 1
                            then error "underflow z"
                            else S ((minX1, maxX1), (minY1, maxY1), (minZ1, maxZ1 - newLength)) ((minX1, maxX1), (minY1, maxY1), (minZ1 + newLength, maxZ1))
                    else NC

countPoints :: Range3 -> Integer
countPoints ((minX, maxX), (minY, maxY), (minZ, maxZ)) =
  (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)

count ::
  [(Bool, Range3)] -> -- Make sure to reverse this list, because the later ones get priority
  Range3 ->
  [(Bool, Range3)] ->
  Integer
count _ _ [] = 0
count allRs exploring xs@((isOn, h) : t) =
  case comparison exploring h of
    LeftInRight -> if isOn then countPoints exploring else 0
    NoContact -> count allRs exploring t
    SomeContact -> sum $ fmap (\e -> count allRs e xs) $ binSplit exploring

count2 ::
  Range3 ->
  [(Bool, Range3)] -> -- Make sure to reverse this list, because the later ones get priority
  Integer
count2 _ [] = 0
count2 exploring xs@((isOn, h) : t) =
  case compOrSplit exploring h of
    LIR -> if isOn then countPoints exploring else 0
    NC -> count2 exploring t
    S a b -> count2 a xs + count2 b xs

b (a, b) = (max a (-50), min b 50)

answer2 :: _ -> _
answer2 xs =
  let ready = fmap (fmap (\(x, y, z) -> (order x, order y, order z))) $ reverse xs
      mag = 17 -- none of the big cubes live within +/-2^15 of the center
      d = (-2 ^ mag, 2 ^ mag - 1)
   in count2 (d, d, d) ready

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $
      binSplit ((5, 20), (3, 18), (-5, 10))
        `shouldBe` [ ((5, 12), (3, 10), (-5, 2)),
                     ((13, 20), (3, 10), (-5, 2)),
                     ((5, 12), (11, 18), (-5, 2)),
                     ((5, 12), (3, 10), (3, 10)),
                     ((13, 20), (11, 18), (-5, 2)),
                     ((13, 20), (3, 10), (3, 10)),
                     ((5, 12), (11, 18), (3, 10)),
                     ((13, 20), (11, 18), (3, 10))
                   ]
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
