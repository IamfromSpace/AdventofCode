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

bits :: APC () [Bool]
bits =
  ( (AP.many AP.digit)
      >>^ ( \xs ->
              fmap
                ( \case
                    '1' -> True
                    '0' -> False
                )
                xs
          )
  )

parse1 :: String -> [[Bool]]
parse1 s =
  AP.parseImpure
    ( AP.linesOf bits
    )
    s

parse2 :: String -> _
parse2 = parse1

toNum xs = sum $ zipWith (\c e -> 2 ^ e * (if c then 1 else 0)) (reverse xs) [0 ..]

a :: [[Bool]] -> _
a xs =
  let transposed :: [[Bool]]
      transposed = List.transpose xs
      half = length xs `div` 2
      gamma = fmap (\row -> length (filter id row) > half) transposed
      epsilon = fmap (\row -> length (filter not row) > half) transposed
   in toNum gamma * toNum epsilon

answer1 :: _ -> _
answer1 = a

-- Answer 2 is substantially rewritten!

-- | The world's most useless data structure!
--
-- This tree has multiple lists of booleans, tracking if there are more Trues
-- or Falses at each level of the tree (aka: index of the list).  This means we
-- can insert an item in O(log(n)) and O(log(n)) of finding the most/least path
-- (where n is the length of the boolean list).
--
-- For example, consider:
-- TFT
-- TFF
-- TTT
-- FFT
--
-- If we build a LeastMostBoolListTree out of this we get:
-- @
-- Branch 2
--   (Branch (-1)
--     (Leaf [True])
--     (Branch 0
--       (Leaf [])
--       (Leaf [])
--     )
--   )
--   (Leaf [False, True])
-- @
--
-- We can rebuild the original entries by walking through both True and False
-- paths, appending the respective value to the entries of the branch (and
-- leaves contain only a single entry).
--
-- To find the "most" path we'd first follow the True path (since there are 2
-- more Trues than Falses), then follow the False path (since there's one more
-- False than True) and then we'd default to the True path (since it's a tie)
-- and then find the leaf with the remainder of the entries (in this case,
-- none).  This all comes together as [True, False, True].
--
-- Our "least" path follows the False path (since there are 2 more Trues than
-- Falses), then hits a leaf, so all items in the Leaf are accepted. This
-- combines to yield [False, False, True].
data LeastMostBoolListTree
  = Branch
      Int -- How many more True entries there are than False Entrie
      LeastMostBoolListTree -- The True path
      LeastMostBoolListTree -- The False path
  | Leaf [Bool]
  deriving (Show, Eq)

empty :: LeastMostBoolListTree
empty = Leaf []

insert :: [Bool] -> LeastMostBoolListTree -> LeastMostBoolListTree
-- Empty list has no effect on the leaves (though, it may have not been empty
-- when it was originally inserted, meaning it still affects the counts in
-- parent branches).  This happens when you have something like [True] and
-- [True, False].  The former influences counts, but is otherwise "lost."
insert [] s = s
insert (True : bs1) (Leaf (True : bs2)) = Branch 2 (insert bs1 (Leaf bs2)) (Leaf [])
insert (False : bs1) (Leaf (False : bs2)) = Branch (-2) (Leaf []) (insert bs1 (Leaf bs2))
insert (True : bs1) (Leaf (False : bs2)) = Branch 0 (Leaf bs1) (Leaf bs2)
insert (False : bs1) (Leaf (True : bs2)) = Branch 0 (Leaf bs2) (Leaf bs1)
insert bs (Leaf []) = Leaf bs
insert (True : bs) (Branch i a b) = Branch (i + 1) (insert bs a) b
insert (False : bs) (Branch i a b) = Branch (i -1) a (insert bs b)

-- Empty trees return []
most :: LeastMostBoolListTree -> [Bool]
most (Leaf bs) = bs
most (Branch i a b) = if i >= 0 then True : most a else False : most b

-- Empty trees return [], if the lesser branch is empty, the chain of booleans ends
least :: LeastMostBoolListTree -> [Bool]
least (Leaf bs) = bs
least (Branch i a b) = if i < 0 then True : least a else False : least b

oxygen :: [[Bool]] -> [Bool]
oxygen = most . List.foldr insert empty

co2 :: [[Bool]] -> [Bool]
co2 = least . List.foldr insert empty

answer2 :: _ -> _
answer2 xs = toNum (oxygen xs) * toNum (co2 xs)

show1 :: Show _a => _a -> String
show1 = show

show2 :: Show _a => _a -> String
show2 = show

tests :: _
tests = do
  describe "pure components" $ do
    it "should parse" $ parse1 undefined `shouldBe` undefined
  describe "part 1" $ do
    let p2 = Util.autoFileTest (answer2 . parse2)
    it "example 1" $ p2 "./ex2.txt" 230
    it "example 1" $ Util.autoFileTest (toNum . oxygen . parse2) "./ex2.txt" (23 :: Int)
    it "example 1" $ Util.autoFileTest (toNum . co . parse2) "./ex2.txt" (10 :: Int)
