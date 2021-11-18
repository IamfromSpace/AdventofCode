{-# LANGUAGE Arrows #-}

module Lib where

import AdventOfCode.ArrowParser (APC, (!>>), (>>!))
import qualified AdventOfCode.ArrowParser as AP
import AdventOfCode.Util (Vector (..), multiLines)
import qualified AdventOfCode.Util as Util
import Control.Applicative (pure, (<*>), (<|>))
import qualified Control.Applicative as App
import Control.Arrow
  ( arr,
    returnA,
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

-- available doesn't matter!
data Node = Node (Vector (Integer, Integer)) Integer Integer Integer deriving (Show, Eq, Ord)

parseNode :: APC () Node
parseNode = proc () -> do
  AP.string "/dev/grid/node-x" -< ()
  xStr <- AP.many1 AP.digit -< ()
  AP.string "-y" -< ()
  yStr <- AP.many1 AP.digit -< ()
  AP.many1 (AP.token ' ') -< ()
  sizeStr <- AP.many1 AP.digit -< ()
  AP.token 'T' -< ()
  AP.many1 (AP.token ' ') -< ()
  usedStr <- AP.many1 AP.digit -< ()
  AP.token 'T' -< ()
  AP.many1 (AP.token ' ') -< ()
  availStr <- AP.many1 AP.digit -< ()
  AP.many1 (AP.tokenIs (not . (==) '\n')) -< ()
  returnA -< Node (Vector (read xStr, read yStr)) (read sizeStr) (read usedStr) (read availStr)

restOfLine :: APC () ()
restOfLine = (AP.many1 (AP.tokenIs (not . (==) '\n')) &&& AP.token '\n') >>^ const ()

parseDf :: APC () [Node]
parseDf = proc () -> do
  restOfLine -< ()
  restOfLine -< ()
  AP.linesOf parseNode -< ()

parse1 :: String -> _
parse1 = either (error . show) id . AP.parse parseDf

parse2 :: String -> _
parse2 = parse1

isPair :: Node -> Node -> Bool
isPair a@(Node _ _ usedA _) b@(Node _ _ _ availB) =
  usedA /= 0 && a /= b && usedA <= availB

allPairs :: [Node] -> Int
allPairs ns = length $ Set.fromList $ filter (uncurry isPair) $ App.liftA2 (,) ns ns

-- 32:10
answer1 :: _ -> _
answer1 = allPairs

type V2 = Vector (Integer, Integer)

type Grid = Map (Vector (Integer, Integer)) (Integer, Integer)

type State = (Vector (Integer, Integer), Grid)

type EmptyState = (V2, V2, Grid)

asGrid :: [Node] -> Grid
asGrid = foldr (\(Node v size used _) m -> Map.insert v (size, used) m) mempty

subHeuristic :: V2 -> Sum Integer
subHeuristic = pure . Util.manLen

couldMvIfAdjacentsEmpty :: Grid -> V2 -> Bool
couldMvIfAdjacentsEmpty grid v =
  case Map.lookup v grid of
    Nothing -> False
    Just (_, used) ->
      not $
        (==) 0 $
          length $
            Maybe.mapMaybe
              ( \d ->
                  let v' = d <> v
                   in case Map.lookup v' grid of
                        Nothing -> Nothing
                        Just (size, _) -> if used > size then Nothing else Just v'
              )
              [Vector (1, 0), Vector (0, 1), Vector (-1, 0), Vector (0, -1)]

subMvs :: Grid -> Integer -> V2 -> [V2]
subMvs grid used v =
  Maybe.mapMaybe
    ( \d ->
        let v' = d <> v
         in case Map.lookup v' grid of
              Nothing -> Nothing
              Just (size, _) -> if used > size then Nothing else Just v'
    )
    [Vector (1, 0), Vector (0, 1), Vector (-1, 0), Vector (0, -1)]

-- Our heuristic is A* where we find the optimal path assuming all nodes were
-- empty (except the goal state)
heuristic :: State -> Sum Integer
heuristic (goal, grid) =
  -- TODO: A* may have a bug where it doesn't test the first state
  if goal == Vector (0, 0)
    then 0
    else
      let used = maybe (error "Our goal was not on the grid!") snd $ Map.lookup goal grid
       in maybe
            -- TODO: A* should possibly be able to explicily signal dead ends
            10000
            fst
            $ Util.aStar2 subHeuristic (fmap (flip Util.AStarStepOption2 1) . subMvs grid used) goal

heuristicSimple :: State -> Sum Integer
heuristicSimple = pure . Util.manLen . fst

preCompute :: Grid -> Integer -> Map V2 (Sum Integer)
preCompute grid used =
  -- sheer stupidity
  -- TODO: explore has a bug where the initial position doesn't have a cost of 0
  Map.insert mempty mempty $
    Util.explore 10000 (fmap (flip Util.AStarStepOption2 1) . subMvs grid used) mempty

heuristicPreComputed :: Map V2 (Sum Integer) -> State -> Sum Integer
heuristicPreComputed preCalc (v, _) = Maybe.fromMaybe (error "preComputed map incomplete!") $ Map.lookup v preCalc

mv :: State -> Vector (Integer, Integer) -> Vector (Integer, Integer) -> Maybe State
mv (goal, grid) from dir =
  let to = from <> dir
   in case (Map.lookup from grid, Map.lookup to grid) of
        (Just (sizeF, usedF), Just (sizeT, usedT)) ->
          if usedF <= (sizeT - usedT) && (from /= goal || usedT == 0)
            then Just (if goal == from then to else goal, Map.insert from (sizeF, 0) $ Map.insert to (sizeT, usedF + usedT) grid)
            else Nothing
        _ -> Nothing

oneNodeMvs :: State -> Vector (Integer, Integer) -> [State]
oneNodeMvs state from =
  Maybe.mapMaybe (mv state from) [Vector (1, 0), Vector (0, 1), Vector (-1, 0), Vector (0, -1)]

allMvs :: State -> [State]
allMvs s@(_, grid) = concatMap (oneNodeMvs s) $ Map.keys grid

emptyMvs :: EmptyState -> [EmptyState]
emptyMvs (empty, goal, grid) =
  Maybe.mapMaybe
    ( \d ->
        let from = empty <> d
            sizeEmpty = maybe (error "empty not on grid!") fst $ Map.lookup empty grid
         in case Map.lookup from grid of
              Nothing -> Nothing
              Just (sizeF, used) ->
                if used <= sizeEmpty
                  then
                    Just
                      ( from,
                        if from == goal then empty else goal,
                        Map.insert from (sizeF, 0) $ Map.insert empty (sizeEmpty, used) grid
                      )
                  else Nothing
    )
    [Vector (1, 0), Vector (0, 1), Vector (-1, 0), Vector (0, -1)]

emptyHeuristic :: EmptyState -> Sum Integer
emptyHeuristic (Vector (ex, ey), g@(Vector (gx, gy)), _) =
  -- We calculate both the minimum goal distance and the minimum distance to
  -- get the empty node to an advancing adjacent position.  The cost of the
  -- goal movement then includes the fastest possible "shuffle" where the empty
  -- state is constantly rotating around it to move it forward.  Note that
  -- while it takes 5 steps to advance horizontally or vertical, it only takes
  -- 6 steps to do a diagonal move (which is a h and v move for two total
  -- moves).
  let ml = Util.manLen g
      toXAdjacent = if gx /= 0 then Just (abs (ex - (gx - 1)) + abs (ey - gy)) else Nothing
      toYAdjacent = if gy /= 0 then Just (abs (ex - gx) + abs (ey - (gy - 1))) else Nothing
   in Sum $
        (if ml <= 1 then ml else (ml - 1) * 5 + 1)
          +
          -- If they're both Nothing, we must be in the goal state
          Maybe.fromMaybe 0 (App.liftA2 min toXAdjacent toYAdjacent)

answer2' :: _ -> _ -> _ -> _
answer2' empty goal nodes =
  let grid = asGrid nodes
   in getSum $
        maybe (error "grid was unsolveable!") fst $
          Util.aStar2
            emptyHeuristic
            (fmap (flip Util.AStarStepOption2 1) . emptyMvs)
            (empty, goal, grid)

-- There are some shortcuts we can leverage in our solution:
--   - The final node is 92T Max, the target used data is 72T, and there are no
--     non empty nodes with < 20T, meaning that our target can _never_ merge,
--     even if there's enough room in the adjacent node.  If it ever merges,
--     we'll never be able to fit the data into the final target.
--   - Literally every node _can_ hold the final payload (by previous role it's
--     always exactly 72T).  This means that manhattan length is always the
--     best heuristic if we consider a path while all nodes are empty.
--   - There are no nodes that _cannot_ move if their neighbors were empty.
--     There are certainly nodes that cannot move, but the proof of such is not
--     simple.  For example (8,14) through (29, 14) are all stuck in place, but
--     there are a few steps of deduction here.  (29,14) can only move if
--     (28,14) can, which can only move if (28,14) can, and so on until (8,14)
--     which can't move (other than backwards, which is a contradiction). A*
--     will be mostly unbothered by this, because it won't attempt to explore
--     states where they move (since they can't).  However, I can't think of a
--     way to inform our lower bound heuristic that certain paths are simply
--     untraversable.
--   - No nodes can mv into a non-empty node........only the empty node "moves"
--   - Other than the very large nodes, all nodes can fit the data of all other
--     nodes, this means that basically any swap can be done at any time!
--
-- 10000:00:00
-- After all that...I just solved it by hand, one million years later, sigh.
answer2 :: _ -> _
answer2 = answer2' (Vector (11, 22)) (Vector (29, 0))

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex :: String
ex = "junk\nFilesystem            Size  Used  Avail  Use%\n/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"

tests :: _
tests = do
  describe "pure components" $ do
    it "should solve pt2" $ answer2' (Vector (1, 1)) (Vector (2, 0)) (parse1 ex) `shouldBe` 7
    it "should solve via sub A*" $ heuristic (Vector (2, 0), asGrid (parse1 ex)) `shouldBe` 2
    it "should return 0 in sub A* when solved" $ heuristic (Vector (0, 0), asGrid (parse1 ex)) `shouldBe` 0
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 5" $ p2 "./ex2_5.txt" undefined
