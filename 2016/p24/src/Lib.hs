module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (Vector (..), multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow
  ( arr,
    (&&&),
    (***),
    (<+>),
    (<<<),
    (>>>),
    (>>^),
    (|||),
  )
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Foldable (toList)
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
import Prelude hiding (init, lookup, map, (++))

type V2 = Vector (Integer, Integer)

parse1 ::
  String ->
  ( V2, -- Start
    Map V2 Integer, -- targets
    Set V2 -- not walls
  )
parse1 =
  Util.parseGrid
    ( \p char (i, m, s) ->
        case char of
          '.' -> (i, m, Set.insert (Vector p) s)
          '0' -> (Vector p, m, Set.insert (Vector p) s)
          '1' -> (i, Map.insert (Vector p) 1 m, Set.insert (Vector p) s)
          '2' -> (i, Map.insert (Vector p) 2 m, Set.insert (Vector p) s)
          '3' -> (i, Map.insert (Vector p) 3 m, Set.insert (Vector p) s)
          '4' -> (i, Map.insert (Vector p) 4 m, Set.insert (Vector p) s)
          '5' -> (i, Map.insert (Vector p) 5 m, Set.insert (Vector p) s)
          '6' -> (i, Map.insert (Vector p) 6 m, Set.insert (Vector p) s)
          '7' -> (i, Map.insert (Vector p) 7 m, Set.insert (Vector p) s)
          '#' -> (i, m, s)
          c -> error ("bad parse: " <> [c])
    )
    mempty

parse2 :: String -> _
parse2 = parse1

type State =
  ( Set Integer, --  Collected
    V2 --  Current Position
  )

heuristic' :: Map V2 Integer -> State -> Sum Integer
heuristic' targets (collected, Vector (x, y)) =
  -- Could be more advanced, but it can just be the sum of minimimum from here,
  -- because they could be all right next to eachother.  So we have to try all
  -- permutations--which may be worth it!
  case fmap (\(Vector (x2, y2), _) -> abs (x - x2) + abs (y - y2)) $
    filter (\(_, i) -> not (Set.member i collected)) $ Map.toList targets of
    [] -> 0
    x -> pure $ minimum x

nextPossible :: Set V2 -> Map V2 Integer -> State -> [State]
nextPossible walkable targets (collected, p) =
  Maybe.mapMaybe
    ( \d ->
        let p' = p <> Vector d
         in if Set.member p' walkable
              then
                Just
                  ( collected
                      <> maybe
                        mempty
                        Set.singleton
                        (Map.lookup p' targets),
                    p'
                  )
              else Nothing
    )
    [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- 32:44 - 56 on the leaderboard!
answer1 ::
  ( V2, -- Start
    Map V2 Integer, -- targets
    Set V2 -- not walls
  ) ->
  _
answer1 (start, targets, walkable) =
  maybe (error "no path") (getSum . fst) $
    Util.aStar2
      (heuristic' targets)
      (fmap (flip Util.AStarStepOption2 1) . nextPossible walkable targets)
      (mempty, start)

heuristic2 :: V2 -> Map V2 Integer -> State -> Sum Integer
heuristic2 (Vector (sx, sy)) targets (collected, Vector (x, y)) =
  -- Could be more advanced, but it can just be the sum of minimimum from here,
  -- because they could be all right next to eachother.  So we have to try all
  -- permutations--which may be worth it!
  case fmap (\(Vector (x2, y2), _) -> abs (x - x2) + abs (y - y2)) $
    filter (\(_, i) -> not (Set.member i collected)) $ Map.toList targets of
    [] -> pure (abs (x - sx) + abs (y - sy))
    x -> pure $ minimum x

-- 36:22 - 60 on the leaderboard!
answer2 ::
  ( V2, -- Start
    Map V2 Integer, -- targets
    Set V2 -- not walls
  ) ->
  _
answer2 (start, targets, walkable) =
  maybe (error "no path") (getSum . fst) $
    Util.aStar2
      (heuristic2 start targets)
      (fmap (flip Util.AStarStepOption2 1) . nextPossible walkable targets)
      (mempty, start)

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
    it "example 5" $ p2 "./ex2_5.txt" undefined
