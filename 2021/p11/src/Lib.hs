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

parse1 :: String -> Map V2 (Maybe Int)
parse1 = Util.parseGrid (\v c m -> Map.insert (Util.Vector v) (Just (read [c])) m) mempty

parse2 :: String -> _
parse2 = parse1

neighbors :: [V2]
neighbors = fmap Util.Vector [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

processFlashes :: Set V2 -> Map V2 (Maybe Int) -> Map V2 (Maybe Int)
processFlashes toConsider m =
  case Set.minView toConsider of
    Nothing -> m
    Just (v, nextTC) ->
      case Map.lookup v m of
        Nothing -> processFlashes nextTC m
        Just Nothing -> processFlashes nextTC m
        Just (Just x) ->
          if x > 9
            then
              let toCheck = fmap ((<>) v) neighbors
                  mNext =
                    List.foldl
                      ( \m' v' ->
                          Map.alter
                            ( \case
                                Nothing -> Nothing
                                Just Nothing -> Just Nothing
                                Just (Just x) -> Just (Just (x + 1))
                            )
                            v'
                            m'
                      )
                      (Map.insert v Nothing m)
                      toCheck
               in processFlashes (Set.fromList toCheck <> nextTC) mNext
            else processFlashes nextTC m

step' :: Map V2 (Maybe Int) -> (Int, Map V2 (Maybe Int))
step' m =
  Map.foldlWithKey
    ( \(c, m') v e ->
        case e of
          Nothing -> (c + 1, Map.insert v (Just 0) m')
          x -> (c, Map.insert v x m')
    )
    (0, mempty)
    $ processFlashes (Set.fromList $ Map.keys m) (Map.map (fmap ((+) 1)) m)

step :: Int -> Int -> Map V2 (Maybe Int) -> Int
step 0 count _ = count
step n count m =
  let (nextC, nextM) = step' m
   in step (n - 1) (count + nextC) nextM

answer1 :: Map V2 (Maybe Int) -> _
answer1 = step 100 0

step2 :: Int -> Map V2 (Maybe Int) -> Int
step2 n m =
  let (_, nextM) = step' m
   in if all ((==) (Just 0)) (Map.elems nextM) then n else step2 (n + 1) nextM

answer2 :: Map V2 (Maybe Int) -> _
answer2 = (+) 1 . step2 0

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

tests :: _
tests = do
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" 1656
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex1_1.txt" 195
