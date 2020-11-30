{-# LANGUAGE ViewPatterns #-}

module Lib where

import qualified AdventOfCode.Util as Util
import AdventOfCode.Util
       (AStarStepOption2(..), Manhattan(..), Vector(..), manLen)
import Control.Applicative ((<*>), (<|>), pure)
import qualified Control.Applicative as App
import qualified Control.Monad.State.Lazy as Stae
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 ()
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding ((++), init, lookup, map, subtract)

parse1 :: String -> Integer
parse1 = read

parse2 :: String -> _
parse2 = parse1

isWall :: Integer -> Vector (Integer, Integer) -> Bool
isWall favNum (getVector -> (x, y)) =
    x < 0 ||
    y < 0 ||
    ((Bits.popCount (x * x + 3 * x + 2 * x * y + y + y * y + favNum)) `mod` 2 ==
     1)

baseOptions :: [Vector (Integer, Integer)]
baseOptions = [Vector (0, 1), Vector (0, (-1)), Vector (1, 0), Vector ((-1), 0)]

options ::
       Integer
    -> Vector (Integer, Integer)
    -> [AStarStepOption2 (Vector (Integer, Integer)) (Sum Integer)]
options favNum p =
    fmap (\x -> AStarStepOption2 x (Sum 1)) $
    filter (not . isWall favNum) $ fmap (<> p) baseOptions

subtract ::
       Vector (Integer, Integer)
    -> Vector (Integer, Integer)
    -> Vector (Integer, Integer)
subtract (getVector -> (x1, y1)) (getVector -> (x2, y2)) =
    Vector (x2 - x1, y2 - y1)

lowerBound ::
       Vector (Integer, Integer) -> Vector (Integer, Integer) -> Sum Integer
lowerBound target p = Sum (manLen (target `subtract` p))

-- 00:19:33.467 - Would have been 74 on the leaderboard!
-- (It's a bit of dumb luck that my original incorrect (0,0) starting position
-- still gave the correct answer here)
answer1 :: _ -> _
answer1 favNum =
    fmap fst $
    Util.aStar2 (lowerBound (Vector (31, 39))) (options favNum) (Vector (1, 1))

data Cell
    = Wall
    | Visit

renderCell :: Cell -> Char
renderCell c =
    case c of
        Wall -> '#'
        Visit -> '.'

toMap :: Ord a => b -> Set a -> Map a b
toMap x = Set.foldl' (\m k -> Map.insert k x m) mempty

upgrade :: Integer -> Map (Integer, Integer) Cell -> Map (Integer, Integer) Cell
upgrade favNum mmmm =
    List.foldl' (\m k -> Map.insert k Wall m) mmmm $
    filter (isWall favNum . Vector) $ App.liftA2 (,) [0 .. 25] [0 .. 25]

--01:26:16.867
answer2 :: _ -> _
answer2 favNum =
    Map.size $ Util.explore (Sum 50) (options favNum) (Vector (1, 1))
   {-
    Util.prettyPrintPointMap ' ' renderCell $
    upgrade favNum $
    toMap Visit $
    Set.fromList $
    fmap getVector $
    toList $ find favNum (Set.fromList [Vector (0, 0)]) [(0, Vector (0, 0))]
    -}

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

ex1_1 :: _
ex1_1 = undefined

ex1_2 :: _
ex1_2 = undefined

ex1_3 :: _
ex1_3 = undefined

ex1_4 :: _
ex1_4 = undefined

ex1_5 :: _
ex1_5 = undefined

ex2_1 :: _
ex2_1 = undefined

ex2_2 :: _
ex2_2 = undefined

ex2_3 :: _
ex2_3 = undefined

ex2_4 :: _
ex2_4 = undefined

ex2_5 :: _
ex2_5 = undefined
