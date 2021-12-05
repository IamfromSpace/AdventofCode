module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow (arr, returnA, (&&&), (***), (<+>), (<<<), (>>>), (>>^), (^>>), (|||))
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

plays :: APC () [Int]
plays = AP.sepBy1 AP.decimal (AP.token ',')

boardLine :: APC () [Int]
boardLine = AP.optional (AP.token ' ') !>> AP.sepBy1 AP.decimal (AP.many1 (AP.token ' '))

board :: APC () (Map Int Int)
board =
  (const (5, ()) ^>> AP.count (boardLine >>! AP.token '\n')) >>^ (Map.fromList . flip zip [0 ..] . concat)

boards :: APC () [Map Int Int]
boards = AP.sepBy1 board (AP.token '\n')

input :: APC () ([Int], [Map Int Int])
input = (plays >>! AP.string "\n\n") &&& boards

-- Cleaned up substantially
parse1 :: String -> _
parse1 = AP.parseImpure input

parse2 :: String -> _
parse2 = parse1

winners :: [[Int]]
winners =
  fmap
    (\(a, b) -> fmap (\i -> a * i + b) [0 .. 4])
    (fmap ((,) 1) [0, 5 .. 20] <> fmap ((,) 5) [0 .. 4])

-- Cleaned up substantially
hasWon :: Set Int -> Bool
hasWon s =
  any (all (flip Set.member s)) winners

playBoard1 :: Map Int Int -> Int -> Set Int -> Set Int
playBoard1 board called marked =
  case Map.lookup called board of
    Nothing -> marked
    Just i -> Set.insert i marked

play :: [Map Int Int] -> [Int] -> [Set Int] -> (Int, Map Int Int, Set Int)
play _ [] _ = error "no winner"
play boards (called : t) marks =
  let nextMarks = zipWith (\b m -> (b, playBoard1 b called m)) boards marks
   in case filter (hasWon . snd) nextMarks of
        [] -> play boards t (fmap snd nextMarks)
        [(b, m)] -> (called, b, m)
        _ -> error "too many winners!"

score :: (Int, Map Int Int, Set Int) -> Int
score (called, board, marks) =
  called * (sum $ fmap (\(label, pos) -> if Set.member pos marks then 0 else label) $ Map.toList board)

answer1 :: _ -> _
answer1 (plays, boards) = score $ play boards plays (fmap (const mempty) boards)

play2 :: [Int] -> [(Map Int Int, Set Int)] -> (Int, Map Int Int, Set Int)
play2 [] _ = error "no winner"
play2 (called : t) boardsAndMarks =
  let nextMarks = fmap (\(b, m) -> (b, playBoard1 b called m)) boardsAndMarks
   in case (filter (hasWon . snd) nextMarks, length boardsAndMarks) of
        ([(b, m)], 1) -> (called, b, m)
        _ -> play2 t (filter (not . hasWon . snd) nextMarks)

answer2 :: _ -> _
answer2 (plays, boards) = score $ play2 plays (fmap (\b -> (b, mempty)) boards)

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex =
  Map.fromList $
    zip
      [ 22 :: Int,
        13,
        17,
        11,
        0,
        8,
        2,
        23,
        4,
        24,
        21,
        9,
        14,
        16,
        7,
        6,
        10,
        3,
        18,
        5,
        1,
        12,
        20,
        15,
        19
      ]
      [0 .. 24]

tests :: _
tests = do
  describe "pure components" $ do
    it "should win against a single board" $ play [ex] [22, 13, 17, 11, 0] mempty `shouldBe` (0, ex, Set.fromList [0 .. 4])
    it "should play once" $ playBoard1 ex 22 mempty `shouldBe` Set.fromList [0]
    it "should play twice" $ playBoard1 ex 13 (Set.fromList [0]) `shouldBe` Set.fromList [0, 1]
    it "detect win" $ hasWon (Set.fromList [0 .. 4]) `shouldBe` True
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
