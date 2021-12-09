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

parse1 :: String -> [((Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char), (Set Char, Set Char, Set Char, Set Char))]
parse1 s =
  fmap
    ( \line -> case words line of
        [a, b, c, d, e, f, g, h, i, j, "|", x, y, z, zz] -> ((Set.fromList a, Set.fromList b, Set.fromList c, Set.fromList d, Set.fromList e, Set.fromList f, Set.fromList g, Set.fromList h, Set.fromList i, Set.fromList j), (Set.fromList x, Set.fromList y, Set.fromList z, Set.fromList zz))
    )
    $ lines s

parse2 :: String -> _
parse2 = parse1

is1478 x = Set.size x == 2 || Set.size x == 4 || Set.size x == 3 || Set.size x == 7

count1478 :: _ -> _
count1478 (_, (a, b, c, d)) = length $ filter is1478 [a, b, c, d]

answer1 :: [((Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char), (Set Char, Set Char, Set Char, Set Char))] -> _
answer1 xs = sum $ fmap (count1478) xs

testCombo' :: _ -> [Char] -> Bool
testCombo' s rule =
  Maybe.isJust (toDigit s rule)

testCombo :: _ -> [Char] -> Bool
testCombo ((a, b, c, d, e, f, g, h, i, j), (x, y, z, zz)) combo = all (flip testCombo' combo) [a, b, c, d, e, f, g, h, i, j, x, y, z, zz]

toDigit :: Set Char -> [Char] -> Maybe Int
toDigit s [t, tl, tr, m, bl, br, b] =
  case (Set.member t s, Set.member tl s, Set.member tr s, Set.member m s, Set.member bl s, Set.member br s, Set.member b s) of
    (True, True, True, False, True, True, True) -> Just 0
    (False, False, True, False, False, True, False) -> Just 1
    (True, False, True, True, True, False, True) -> Just 2
    (True, False, True, True, False, True, True) -> Just 3
    (False, True, True, True, False, True, False) -> Just 4
    (True, True, False, True, False, True, True) -> Just 5
    (True, True, False, True, True, True, True) -> Just 6
    (True, False, True, False, False, True, False) -> Just 7
    (True, True, True, True, True, True, True) -> Just 8
    (True, True, True, True, False, True, True) -> Just 9
    _ -> Nothing
toDigit _ _ = error "bad"

toNumber :: (Set Char, Set Char, Set Char, Set Char) -> _ -> Int
toNumber (a, b, c, d) rule =
  1000 * (Maybe.fromJust $ toDigit a rule)
    + 100 * (Maybe.fromJust $ toDigit b rule)
    + 10 * (Maybe.fromJust $ toDigit c rule)
    + 1 * (Maybe.fromJust $ toDigit d rule)

perms = List.permutations "abcdefg"

-- (fromList "bcefg" (5),fromList "bcdefg" (6),fromList "abcef" (3),fromList "abfg" (4))
-- [?, bfg, a, bfg, d, bfg, ?]
--
-- (fromList "abcdef" (9),fromList "bcdf"(4),fromList "acdef"(2),fromList "bc"(1))
-- [?, d, c, f, g, b, ?]
--
-- (fromList "bcdefg" (9),fromList "bcg"(7),fromList "abcdefg"(8),fromList "gc"(1))
-- [b, ?, gc, ?, ?, gc, ?]

getNum :: ((Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char, Set Char), (Set Char, Set Char, Set Char, Set Char)) -> Int
getNum xs@(_, x) =
  let rule = case filter (testCombo xs) perms of
        (h : _) -> h -- We don't care if it's ambiguous...?
        _ -> error ("dead: " <> show xs)
   in toNumber x rule

answer2 :: _ -> _
answer2 xs = sum $ fmap getNum xs

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex =
  fmap
    ( \line ->
        let [a, b, c, d] = fmap Set.fromList $ words line in (a, b, c, d)
    )
    $ lines "fdgacbe cefdb cefbgd gcbe\nfcgedb cgb dgebacf gc\ncg cg fdcagb cbg\nefabcd cedba gadfec cb\ngecf egdcabf bgf bfgea\ngebdcfa ecba ca fadegcb\ncefg dcbef fcge gbcadfe\ned bcgafe cdgba cbgef\ngbdfcae bgc cg cgb\nfgae cfgab fg bagce"

tests :: _
tests = do
  describe "pure components" $ do
    it "should parse" $ undefined `shouldBe` 61229
  --it "should parse" $ sum (fmap getNum' ex) `shouldBe` 61229
  --it "should parse" $ getNum' (ex !! 0) `shouldBe` 8394
  --it "should parse" $ getNum' (ex !! 1) `shouldBe` 9781
  --it "should parse" $ getNum' (ex !! 2) `shouldBe` 1197
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
