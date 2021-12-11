module Lib where

import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import ArrowParser (APC, APError (..), (!>>), (>>!))
import qualified ArrowParser as AP
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

prog :: APC () ()
prog =
  ((AP.token '[' !>> AP.many prog !>> AP.token ']') >>^ const ())
    <+> ((AP.token '(' !>> AP.many prog !>> AP.token ')') >>^ const ())
    <+> ((AP.token '{' !>> AP.many prog !>> AP.token '}') >>^ const ())
    <+> ((AP.token '<' !>> AP.many prog !>> AP.token '>') >>^ const ())

parse1 :: String -> [String]
parse1 = lines

parse2 :: String -> _
parse2 = parse1

getScore :: String -> Int
getScore s =
  case AP.parse prog s of
    Left (_, UnexpectedToken ')') -> 3
    Left (_, UnexpectedToken ']') -> 57
    Left (_, UnexpectedToken '}') -> 1197
    Left (_, UnexpectedToken '>') -> 25137
    Left _ -> 0
    Right _ -> 0
    _ -> error ("bad " <> s)

answer1 :: [String] -> _
answer1 s = sum $ fmap getScore s

correct :: String -> Int -> Int
correct s score =
  case AP.parse prog s of
    Left (_, UnexpectedEndOfInput ')') -> correct (s <> ")") (5 * score + 1)
    Left (_, UnexpectedEndOfInput ']') -> correct (s <> "]") (5 * score + 2)
    Left (_, UnexpectedEndOfInput '}') -> correct (s <> "}") (5 * score + 3)
    Left (_, UnexpectedEndOfInput '>') -> correct (s <> ">") (5 * score + 4)
    Right _ -> score
    _ -> error ("bad " <> s)

answer2 :: [String] -> _
answer2 s =
  let scores = List.sort $ fmap (flip correct 0) $ filter (\x -> getScore x == 0) s
   in scores !! (length scores `div` 2)

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
