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

parse1 :: String -> Map V2 Int
parse1 = Util.parseGrid (\v c m -> Map.insert (Util.Vector v) (read [c]) m) mempty

parse2 :: String -> _
parse2 = parse1

type V2 = Util.Vector (Integer, Integer)

isLow :: Map V2 Int -> V2 -> Bool
isLow m v =
  let x = Maybe.fromJust $ Map.lookup v m
      ns = fmap (\d -> Maybe.fromMaybe 100 $ Map.lookup (v <> Util.Vector d) m) [(0, 1), (0, -1), (1, 0), (-1, 0)]
   in all (\n -> x < n) ns

answer1 :: Map V2 Int -> _
answer1 m =
  Map.foldlWithKey
    ( \sum v@(Util.Vector _) i ->
        sum + (if isLow m v then i + 1 else 0)
    )
    0
    m

lowPoints :: Map V2 Int -> Set V2
lowPoints m =
  Map.foldlWithKey
    ( \s v _ ->
        if isLow m v then Set.insert v s else s
    )
    mempty
    m

getBasin :: Map V2 Int -> V2 -> Set V2
getBasin m lowPoint =
  Set.fromList $
    Map.keys $
      Util.explore
        (Sum 1000)
        ( \p ->
            fmap (\v -> Util.AStarStepOption2 v 1) $
              Maybe.mapMaybe
                ( \d ->
                    case Map.lookup (p <> (Util.Vector d)) m of
                      Nothing -> Nothing
                      Just 9 -> Nothing
                      Just _ -> Just (p <> (Util.Vector d))
                )
                [(0, 1), (0, -1), (1, 0), (-1, 0)]
        )
        lowPoint

getBasins :: _ -> [V2] -> Set (Set V2)
getBasins m = Set.fromList . fmap (getBasin m)

answer2 :: _ -> _
answer2 m = product $ take 3 $ reverse $ List.sort $ fmap Set.size $ Set.toList $ getBasins m $ Set.toList $ lowPoints m

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

tests :: _
tests = do
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_2.txt" 1134
