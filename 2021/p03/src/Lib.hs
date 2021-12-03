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

bits :: APC () [Bool]
bits =
  ( (AP.many AP.digit)
      >>^ ( \xs ->
              fmap
                ( \case
                    '1' -> True
                    '0' -> False
                )
                xs
          )
  )

parse1 :: String -> [[Bool]]
parse1 s =
  AP.parseImpure
    ( AP.linesOf bits
    )
    s

parse2 :: String -> _
parse2 = parse1

toNum xs = sum $ zipWith (\c e -> 2 ^ e * (if c then 1 else 0)) (reverse xs) [0 ..]

a :: [[Bool]] -> _
a xs =
  let transposed :: [[Bool]]
      transposed = List.transpose xs
      half = length xs `div` 2
      gamma = fmap (\row -> length (filter id row) > half) transposed
      epsilon = fmap (\row -> length (filter not row) > half) transposed
   in toNum gamma * toNum epsilon

answer1 :: _ -> _
answer1 = a

oxygen i xs =
  let paired = fmap (\x -> (x !! i, x)) xs
      tCount = length $ filter fst paired
      fCount =
        length $
          filter
            (not . fst)
            paired
   in if tCount == 1 && fCount <= 1
        then snd $ head $ filter fst paired
        else
          if fCount == 1 && tCount == 0
            then snd $ head $ filter (not . fst) paired
            else
              if tCount >= fCount
                then oxygen (i + 1) (fmap snd (filter fst paired))
                else oxygen (i + 1) (fmap snd (filter (not . fst) paired))

co :: Int -> [[Bool]] -> [Bool]
co i xs =
  let paired = fmap (\x -> (x !! i, x)) xs
      tCount = length $ filter fst paired
      fCount =
        length $
          filter
            (not . fst)
            paired
   in if fCount == 1 && tCount <= 1
        then snd $ head $ filter (not . fst) paired
        else
          if tCount == 1 && fCount == 0
            then snd $ head $ filter fst paired
            else
              if tCount >= fCount
                then co (i + 1) (fmap snd (filter (not . fst) paired))
                else co (i + 1) (fmap snd (filter fst paired))

answer2 :: _ -> _
answer2 xs = uncurry (*) (toNum (oxygen 0 xs), toNum (co 0 xs))

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

tests :: _
tests = do
  describe "pure components" $ do
    it "should parse" $ parse1 undefined `shouldBe` undefined
  describe "part 1" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2.txt" 230
