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

type V2 = Vector (Integer, Integer)

type BB = ((Integer, Integer), (Integer, Integer))

parse1 :: String -> BB
parse1 _ = ((211, 232), (-124, -69))

parse2 :: String -> _
parse2 = parse1

fire :: BB -> V2 -> V2 -> Maybe [V2]
fire bb@((minX, maxX), (minY, maxY)) p v@(Vector (vx, vy)) =
  let p'@(Vector (px', py')) = p <> v
      v' = Vector (if vx == 0 then 0 else if vx > 0 then vx - 1 else vx + 1, vy) ~~ Vector (0, 1)
   in if px' > maxX || py' < minY
        then Nothing
        else
          if px' >= minX && px' <= maxX && py' >= minY && py' <= maxY
            then Just [p']
            else fmap (\ps -> p' : ps) (fire bb p' v')

answer1 :: _ -> _
answer1 bb = maximum $ fmap (maximum . fmap (\(Vector (_, y)) -> y)) $ Maybe.catMaybes $ fmap (fire bb mempty . Vector) $ App.liftA2 (,) [1 .. 100] [1 .. 1000]

answer2 :: _ -> _
answer2 bb = length $ Maybe.catMaybes $ fmap (fire bb mempty . Vector) $ App.liftA2 (,) [1 .. 232] [-124 .. 1000]

show1 :: Show a => a -> String
show1 = Util.aocShow

show2 :: Show a => a -> String
show2 = Util.aocShow

tests :: _
tests = do
  describe "pure components" $ do
    it "should work" $ answer1 ((20, 30), (-10, -5)) `shouldBe` 45
    it "6,3 should work" $ fire ((20, 30), (-10, -5)) mempty (Vector (6, 3)) `shouldBe` (Just (fmap Vector [(6, 3), (11, 5), (15, 6), (18, 6), (20, 5), (21, 3), (21, 0), (21, -4), (21, -9)]))
    it "6,9 should work" $ fire ((20, 30), (-10, -5)) mempty (Vector (6, 9)) `shouldBe` Nothing
  describe "part 1" $ do
    let p1 = Util.autoFileTest (answer1 . parse1)
    it "example 1" $ p1 "./ex1_1.txt" undefined
  describe "part 2" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2_1.txt" undefined
