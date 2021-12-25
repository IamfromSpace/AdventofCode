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

data Dir
  = R
  | D
  deriving (Show, Eq)

type V2 = Vector (Integer, Integer)

parse1 :: String -> _
parse1 =
  Util.parseGrid
    ( \p c m -> case c of
        '>' -> Map.insert p R m
        'v' -> Map.insert p D m
        _ -> m
    )
    mempty

parse2 :: String -> _
parse2 = parse1

{-
 -     01
 - 0 - >>
 - 1 - >>
 -
 -}

step :: (Integer, Integer) -> Map (Integer, Integer) Dir -> Map (Integer, Integer) Dir
step (maxX, maxY) original =
  let down = filter ((==) D . snd) $ Map.toList original
      right = filter ((==) R . snd) $ Map.toList original
      f o =
        foldl
          ( \next ((x, y), d) ->
              case d of
                R ->
                  let x' = (x + 1) `mod` maxX
                   in case Map.lookup (x', y) o of
                        Nothing -> Map.insert (x', y) d next
                        Just _ -> Map.insert (x, y) d next
                D ->
                  let y' = (y + 1) `mod` maxY
                   in case Map.lookup (x, y') o of
                        Nothing -> Map.insert (x, y') d next
                        Just _ -> Map.insert (x, y) d next
          )
          mempty
      afterRs = f original right
      right' = Map.toList afterRs
   in foldl (\m (k, v) -> Map.insert k v m) (f (foldl (\m (k, v) -> Map.insert k v m) afterRs down) down) right'

findEq :: (Map (Integer, Integer) Dir -> Map (Integer, Integer) Dir) -> Integer -> Map (Integer, Integer) Dir -> Integer
findEq f i init =
  let next = f init
   in if next == init
        then i + 1
        else findEq f (i + 1) next

answer boundary = findEq (step boundary) 0

answer1 :: _ -> _
answer1 = answer (139, 137)

answer2 :: _ -> _
answer2 = id

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

ex = "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ answer (10, 9) (parse1 ex) `shouldBe` 58
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
