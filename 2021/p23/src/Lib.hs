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

data State = State
  { one :: Maybe Char,
    two :: Maybe Char,
    as :: [Char],
    three :: Maybe Char,
    bs :: [Char],
    four :: Maybe Char,
    cs :: [Char],
    five :: Maybe Char,
    ds :: [Char],
    six :: Maybe Char,
    seven :: Maybe Char,
    aHome :: Int,
    bHome :: Int,
    cHome :: Int,
    dHome :: Int
  }
  deriving (Show, Eq, Ord)

{-
#############
#...........#
###D#A#C#C###
  #D#C#B#A#
  #D#B#A#C#
  #D#A#B#B#
  #########
 -}

parse1 :: String -> _
parse1 = id

parse2 :: String -> _
parse2 _ =
  State
    { one = Nothing,
      two = Nothing,
      as = ['D', 'D', 'D', 'D'],
      three = Nothing,
      bs = ['A', 'C', 'B', 'A'],
      four = Nothing,
      cs = ['C', 'B', 'A', 'B'],
      five = Nothing,
      ds = ['C', 'A', 'C', 'B'],
      six = Nothing,
      seven = Nothing,
      aHome = 0,
      bHome = 0,
      cHome = 0,
      dHome = 0
    }

-- solved by hand
answer1 :: _ -> _
answer1 = id

costToMove :: Char -> Sum Int
costToMove 'A' = 1
costToMove 'B' = 10
costToMove 'C' = 100
costToMove 'D' = 1000
costToMove _ = error "bad"

canExitTo :: [Char] -> [Maybe Char] -> Bool -> Maybe (Char, [Char], Sum Int)
canExitTo [] _ _ = Nothing
canExitTo (h : t) blocks includesEnd =
  if all Maybe.isNothing blocks
    then Just (h, t, costToMove h * Sum ((if includesEnd then -1 else 0) + 2 * (length blocks - 1) + 5 - length t))
    else Nothing

homeTo :: Int -> Char -> Maybe Char -> [Maybe Char] -> [Char] -> Bool -> Maybe (Int, Sum Int)
homeTo homeCount home (Just h) blocks [] includesEnd =
  if home == h && all Maybe.isNothing blocks
    then Just (homeCount + 1, costToMove h * Sum ((if includesEnd then -1 else 0) + 2 * length blocks + 5 - homeCount))
    else Nothing -- Home doen't match or blocked
homeTo _ _ _ _ _ _ = Nothing -- No one in that spot, or home isn't cleared

getOptions :: State -> [Util.AStarStepOption2 State (Sum Int)]
getOptions x =
  Maybe.catMaybes
    [ fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {one = Nothing, aHome = newHome}) cost)
        $ homeTo (aHome x) 'A' (one x) [two x] (as x) True,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {two = Nothing, aHome = newHome}) cost)
        $ homeTo (aHome x) 'A' (two x) [] (as x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {three = Nothing, aHome = newHome}) cost)
        $ homeTo (aHome x) 'A' (three x) [] (as x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {four = Nothing, aHome = newHome}) cost)
        $ homeTo (aHome x) 'A' (four x) [three x] (as x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {five = Nothing, aHome = newHome}) cost)
        $ homeTo (aHome x) 'A' (five x) [four x, three x] (as x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {six = Nothing, aHome = newHome}) cost)
        $ homeTo (aHome x) 'A' (six x) [five x, four x, three x] (as x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {seven = Nothing, aHome = newHome}) cost)
        $ homeTo (aHome x) 'A' (seven x) [six x, five x, four x, three x] (as x) True,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {one = Nothing, bHome = newHome}) cost)
        $ homeTo (bHome x) 'B' (one x) [two x, three x] (bs x) True,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {two = Nothing, bHome = newHome}) cost)
        $ homeTo (bHome x) 'B' (two x) [three x] (bs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {three = Nothing, bHome = newHome}) cost)
        $ homeTo (bHome x) 'B' (three x) [] (bs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {four = Nothing, bHome = newHome}) cost)
        $ homeTo (bHome x) 'B' (four x) [] (bs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {five = Nothing, bHome = newHome}) cost)
        $ homeTo (bHome x) 'B' (five x) [four x] (bs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {six = Nothing, bHome = newHome}) cost)
        $ homeTo (bHome x) 'B' (six x) [five x, four x] (bs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {seven = Nothing, bHome = newHome}) cost)
        $ homeTo (bHome x) 'B' (seven x) [six x, five x, four x] (bs x) True,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {one = Nothing, cHome = newHome}) cost)
        $ homeTo (cHome x) 'C' (one x) [two x, three x, four x] (cs x) True,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {two = Nothing, cHome = newHome}) cost)
        $ homeTo (cHome x) 'C' (two x) [three x, four x] (cs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {three = Nothing, cHome = newHome}) cost)
        $ homeTo (cHome x) 'C' (three x) [four x] (cs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {four = Nothing, cHome = newHome}) cost)
        $ homeTo (cHome x) 'C' (four x) [] (cs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {five = Nothing, cHome = newHome}) cost)
        $ homeTo (cHome x) 'C' (five x) [] (cs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {six = Nothing, cHome = newHome}) cost)
        $ homeTo (cHome x) 'C' (six x) [five x] (cs x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {seven = Nothing, cHome = newHome}) cost)
        $ homeTo (cHome x) 'C' (seven x) [six x, five x] (cs x) True,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {one = Nothing, dHome = newHome}) cost)
        $ homeTo (dHome x) 'D' (one x) [two x, three x, four x, five x] (ds x) True,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {two = Nothing, dHome = newHome}) cost)
        $ homeTo (dHome x) 'D' (two x) [three x, four x, five x] (ds x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {three = Nothing, dHome = newHome}) cost)
        $ homeTo (dHome x) 'D' (three x) [four x, five x] (ds x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {four = Nothing, dHome = newHome}) cost)
        $ homeTo (dHome x) 'D' (four x) [five x] (ds x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {five = Nothing, dHome = newHome}) cost)
        $ homeTo (dHome x) 'D' (five x) [] (ds x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {six = Nothing, dHome = newHome}) cost)
        $ homeTo (dHome x) 'D' (six x) [] (ds x) False,
      fmap
        (\(newHome, cost) -> Util.AStarStepOption2 (x {seven = Nothing, dHome = newHome}) cost)
        $ homeTo (dHome x) 'D' (seven x) [six x] (ds x) True,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {as = t, one = Just char}) cost)
        $ canExitTo (as x) [one x, two x] True,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {as = t, two = Just char}) cost)
        $ canExitTo (as x) [two x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {as = t, three = Just char}) cost)
        $ canExitTo (as x) [three x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {as = t, four = Just char}) cost)
        $ canExitTo (as x) [three x, four x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {as = t, five = Just char}) cost)
        $ canExitTo (as x) [three x, four x, five x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {as = t, six = Just char}) cost)
        $ canExitTo (as x) [three x, four x, five x, six x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {as = t, seven = Just char}) cost)
        $ canExitTo (as x) [three x, four x, five x, six x, seven x] True,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {bs = t, one = Just char}) cost)
        $ canExitTo (bs x) [one x, two x, three x] True,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {bs = t, two = Just char}) cost)
        $ canExitTo (bs x) [two x, three x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {bs = t, three = Just char}) cost)
        $ canExitTo (bs x) [three x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {bs = t, four = Just char}) cost)
        $ canExitTo (bs x) [four x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {bs = t, five = Just char}) cost)
        $ canExitTo (bs x) [four x, five x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {bs = t, six = Just char}) cost)
        $ canExitTo (bs x) [four x, five x, six x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {bs = t, seven = Just char}) cost)
        $ canExitTo (bs x) [four x, five x, six x, seven x] True,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {cs = t, one = Just char}) cost)
        $ canExitTo (cs x) [one x, two x, three x, four x] True,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {cs = t, two = Just char}) cost)
        $ canExitTo (cs x) [two x, three x, four x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {cs = t, three = Just char}) cost)
        $ canExitTo (cs x) [three x, four x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {cs = t, four = Just char}) cost)
        $ canExitTo (cs x) [four x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {cs = t, five = Just char}) cost)
        $ canExitTo (cs x) [five x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {cs = t, six = Just char}) cost)
        $ canExitTo (cs x) [five x, six x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {cs = t, seven = Just char}) cost)
        $ canExitTo (cs x) [five x, six x, seven x] True,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {ds = t, one = Just char}) cost)
        $ canExitTo (ds x) [one x, two x, three x, four x, five x] True,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {ds = t, two = Just char}) cost)
        $ canExitTo (ds x) [two x, three x, four x, five x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {ds = t, three = Just char}) cost)
        $ canExitTo (ds x) [three x, four x, five x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {ds = t, four = Just char}) cost)
        $ canExitTo (ds x) [four x, five x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {ds = t, five = Just char}) cost)
        $ canExitTo (ds x) [five x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {ds = t, six = Just char}) cost)
        $ canExitTo (ds x) [six x] False,
      fmap
        (\(char, t, cost) -> Util.AStarStepOption2 (x {ds = t, seven = Just char}) cost)
        $ canExitTo (ds x) [six x, seven x] True
    ]

-- Start dumb
heuristic x = if aHome x == 4 && bHome x == 4 && cHome x == 4 && dHome x == 4 then 0 else 1

answer2 :: State -> Int
answer2 = getSum . fst . Maybe.fromJust . Util.aStar2 heuristic getOptions

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
